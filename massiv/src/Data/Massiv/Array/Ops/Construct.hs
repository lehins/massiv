{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Construct
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Construct
  ( -- ** With constant value
    empty
  , singleton
  , replicate
    -- ** With a function
  , makeArray
  , makeArrayLinear
  , makeArrayR
  , makeArrayLinearR
  , makeVectorR
    -- *** Iterating
  , iterateN
  , iiterateN
    -- *** Unfolding
  , unfoldlS_
  -- , unfoldlS
  , iunfoldlS_
  --, iunfoldlS
  , unfoldrS_
  --, unfoldrS
  , iunfoldrS_
  --, iunfoldrS
    -- *** Random
  , uniformArray
  , uniformRangeArray
  , randomArray
  , randomArrayS
  , randomArrayWS
    -- *** Applicative
  , makeArrayA
  , makeArrayAR
  , makeArrayLinearA
    -- ** Enumeration
  , (...)
  , (..:)
  , range
  , rangeStepM
  , rangeStep'
  , rangeInclusive
  , rangeStepInclusiveM
  , rangeStepInclusive'
  , rangeSize
  , rangeStepSize
  , enumFromN
  , enumFromStepN
    -- ** Expansion
  , expandWithin
  , expandWithinM
  , expandWithin'
  , expandOuter
  , expandInner
  ) where

import Control.Applicative hiding (empty)
import Control.Monad (when, void)
import Control.Monad.ST
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
--import Data.Massiv.Array.Delayed.Stream (unfoldr, unfoldrN)
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Prelude hiding (enumFromTo, replicate)
import System.Random.Stateful

-- | Just like `makeArray` but with ability to specify the result representation as an
-- argument. Note the `Data.Massiv.Array.U`nboxed type constructor in the below example.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> makeArrayR U Par (Sz (2 :> 3 :. 4)) (\ (i :> j :. k) -> i * i + j * j == k * k)
-- Array U Par (Sz (2 :> 3 :. 4))
--   [ [ [ True, False, False, False ]
--     , [ False, True, False, False ]
--     , [ False, False, True, False ]
--     ]
--   , [ [ False, True, False, False ]
--     , [ False, False, False, False ]
--     , [ False, False, False, False ]
--     ]
--   ]
--
-- @since 0.1.0
makeArrayR :: Load r ix e => r -> Comp -> Sz ix -> (ix -> e) -> Array r ix e
makeArrayR _ = makeArray
{-# INLINE makeArrayR #-}

-- | Same as `makeArrayLinear`, but with ability to supply resulting representation
--
-- @since 0.3.0
makeArrayLinearR :: Load r ix e => r -> Comp -> Sz ix -> (Int -> e) -> Array r ix e
makeArrayLinearR _ = makeArrayLinear
{-# INLINE makeArrayLinearR #-}

-- | Same as `makeArrayR`, but restricted to 1-dimensional arrays.
--
-- @since 0.1.0
makeVectorR :: Load r Ix1 e => r -> Comp -> Sz1 -> (Ix1 -> e) -> Vector r e
makeVectorR _ = makeArray
{-# INLINE makeVectorR #-}


newtype STA r ix a = STA {_runSTA :: forall s. MArray s r ix a -> ST s (Array r ix a)}

runSTA :: (Mutable r e, Index ix) => Sz ix -> STA r ix e -> Array r ix e
runSTA !sz (STA m) = runST (unsafeNew sz >>= m)
{-# INLINE runSTA  #-}

-- | Similar to `makeArray`, but construct the array sequentially using an `Applicative` interface.
--
-- /Note/ - using `Data.Massiv.Array.Mutable.generateArray` or
-- `Data.Massiv.Array.Mutable.generateArrayS` will always be faster, althought not always possible.
--
--
-- @since 0.2.6
makeArrayA ::
     forall r ix e f. (Mutable r e, Index ix, Applicative f)
  => Sz ix
  -> (ix -> f e)
  -> f (Array r ix e)
makeArrayA !sz f =
  let n = totalElem sz
      go !i
        | i < n =
          liftA2
            (\e (STA st) -> STA (\ma -> unsafeLinearWrite ma i e >> st ma))
            (f (fromLinearIndex sz i))
            (go (i + 1))
        | otherwise = pure (STA (unsafeFreeze Seq))
   in runSTA sz <$> go 0
{-# INLINE makeArrayA  #-}

-- | Same as `makeArrayA`, but with linear index.
--
-- @since 0.4.5
makeArrayLinearA ::
     forall r ix e f. (Mutable r e, Index ix, Applicative f)
  => Sz ix
  -> (Int -> f e)
  -> f (Array r ix e)
makeArrayLinearA !sz f =
  let n = totalElem sz
      go !i
        | i < n =
          liftA2 (\e (STA st) -> STA (\ma -> unsafeLinearWrite ma i e >> st ma)) (f i) (go (i + 1))
        | otherwise = pure (STA (unsafeFreeze Seq))
   in runSTA sz <$> go 0
{-# INLINE makeArrayLinearA  #-}


-- | Same as `makeArrayA`, but with ability to supply result array representation.
--
-- @since 0.2.6
makeArrayAR ::
     forall r ix e f. (Mutable r e, Index ix, Applicative f)
  => r
  -> Sz ix
  -> (ix -> f e)
  -> f (Array r ix e)
makeArrayAR _ = makeArrayA
{-# INLINE makeArrayAR #-}


-- | Sequentially iterate over each cell in the array in the row-major order while continuously
-- aplying the accumulator at each step.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> iterateN (Sz2 2 10) succ (10 :: Int)
-- Array DL Seq (Sz (2 :. 10))
--   [ [ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
--   , [ 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 ]
--   ]
--
-- @since 0.3.0
iterateN :: forall ix e . Index ix => Sz ix -> (e -> e) -> e -> Array DL ix e
iterateN sz f = unfoldrS_ sz $ \a -> let !a' = f a in (a', a')
{-# INLINE iterateN #-}

-- | Same as `iterateN`, but with index aware function.
--
-- @since 0.3.0
iiterateN :: forall ix e . Index ix => Sz ix -> (e -> ix -> e) -> e -> Array DL ix e
iiterateN sz f = iunfoldrS_ sz $ \a ix -> let !a' = f a ix in (a', a')
{-# INLINE iiterateN #-}

-- | Right unfold into a delayed load array. For the opposite direction use `unfoldlS_`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> unfoldrS_ (Sz1 10) (\xs -> (Prelude.head xs, Prelude.tail xs)) ([10 ..] :: [Int])
-- Array DL Seq (Sz1 10)
--   [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 ]
--
-- @since 0.3.0
unfoldrS_ ::
     forall ix e a. Index ix
  => Sz ix
  -> (a -> (e, a))
  -> a
  -> Array DL ix e
unfoldrS_ sz f = iunfoldrS_ sz (\a _ -> f a)
{-# INLINE unfoldrS_ #-}

-- | Right unfold of a delayed load array with index aware function
--
-- @since 0.3.0
iunfoldrS_ ::
     forall ix e a. Index ix
  => Sz ix
  -> (a -> ix -> (e, a))
  -> a
  -> Array DL ix e
iunfoldrS_ sz f acc0 = DLArray {dlComp = Seq, dlSize = sz, dlLoad = load}
  where
    load :: Monad m =>
      Scheduler m () -> Ix1 -> (Ix1 -> e -> m ()) -> (Ix1 -> Sz1 -> e -> m ()) -> m ()
    load _ startAt dlWrite _ =
      void $
      loopM startAt (< totalElem sz + startAt) (+ 1) acc0 $ \ !i !acc ->
        let (e, acc') = f acc $ fromLinearIndex sz (i - startAt)
         in acc' <$ dlWrite i e
    {-# INLINE load #-}
{-# INLINE iunfoldrS_ #-}


-- | Unfold sequentially from the end. There is no way to save the accumulator after
-- unfolding is done, since resulting array is delayed, but it's possible to use
-- `Data.Massiv.Array.Mutable.unfoldlPrimM` to achive such effect.
--
-- @since 0.3.0
unfoldlS_ :: Index ix => Sz ix -> (a -> (a, e)) -> a -> Array DL ix e
unfoldlS_ sz f = iunfoldlS_ sz (const f)
{-# INLINE unfoldlS_ #-}

-- | Unfold sequentially from the right with an index aware function.
--
-- @since 0.3.0
iunfoldlS_ ::
     forall ix e a. Index ix
  => Sz ix
  -> (ix -> a -> (a, e))
  -> a
  -> Array DL ix e
iunfoldlS_ sz f acc0 = DLArray {dlComp = Seq, dlSize = sz, dlLoad = load}
  where
    load :: Monad m =>
      Scheduler m () -> Ix1 -> (Ix1 -> e -> m ()) -> (Ix1 -> Sz1 -> e -> m ()) -> m ()
    load _ startAt dlWrite _ =
      void $
      loopDeepM startAt (< totalElem sz + startAt) (+ 1) acc0 $ \ !i !acc ->
        let (acc', e) = f (fromLinearIndex sz (i - startAt)) acc
         in acc' <$ dlWrite i e
    {-# INLINE load #-}
{-# INLINE iunfoldlS_ #-}


-- | Create an array with random values by using a pure splittable random number generator
-- such as one provided by either [splitmix](https://www.stackage.org/package/splitmix) or
-- [random](https://www.stackage.org/package/random) packages. If you don't have a
-- splittable generator consider using `randomArrayS` or `randomArrayWS` instead.
--
-- Because of the pure nature of the generator and its splitability we are not only able
-- to parallelize the random value generation, but also guarantee that it will be
-- deterministic, granted none of the arguments have changed.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> import System.Random.SplitMix as SplitMix
-- >>> gen = SplitMix.mkSMGen 217
-- >>> randomArray gen SplitMix.splitSMGen SplitMix.nextDouble (ParN 2) (Sz2 2 3) :: Array DL Ix2 Double
-- Array DL (ParN 2) (Sz (2 :. 3))
--   [ [ 0.7383156058619669, 0.39904053166835896, 0.5617584038393628 ]
--   , [ 0.7218718218678238, 0.7006722805067258, 0.7225894731396042 ]
--   ]
--
-- >>> import Data.Massiv.Array
-- >>> import System.Random as System
-- >>> gen = System.mkStdGen 217
-- >>> randomArray gen System.split System.random (ParN 2) (Sz2 2 3) :: Array DL Ix2 Double
-- Array DL (ParN 2) (Sz (2 :. 3))
--   [ [ 0.2616843941380331, 0.600959468331641, 0.4382415961606372 ]
--   , [ 0.27812817813217605, 0.2993277194932741, 0.2774105268603957 ]
--   ]
--
-- @since 1.0.0
randomArray ::
     forall ix e g. Index ix
  => g -- ^ Initial random value generator
  -> (g -> (g, g))
     -- ^ A function that can split a generator in two independent generators
  -> (g -> (e, g))
     -- ^ A function that produces a random value and the next generator
  -> Comp -- ^ Computation strategy.
  -> Sz ix -- ^ Resulting size of the array.
  -> Array DL ix e
randomArray gen splitGen nextRandom comp sz = unsafeMakeLoadArray comp sz Nothing load
  where
    !totalLength = totalElem sz
    load :: Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ()
    load scheduler startAt writeAt =
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        let slackStartAt = slackStart + startAt
            writeRandom k genII =
              let (e, genII') = nextRandom genII
               in genII' <$ writeAt k e
        genForSlack <-
          loopM startAt (< slackStartAt) (+ chunkLength) gen $ \start genI -> do
            let (genI0, genI1) =
                  if numWorkers scheduler == 1
                    then (genI, genI)
                    else splitGen genI
            scheduleWork_ scheduler $
              void $ loopM start (< start + chunkLength) (+ 1) genI0 writeRandom
            pure genI1
        when (slackStartAt < totalLength + startAt) $
          scheduleWork_ scheduler $
          void $ loopM slackStartAt (< totalLength + startAt) (+ 1) genForSlack writeRandom
{-# INLINE randomArray #-}


-- | Generate a random array where all elements are sampled from a uniform distribution.
--
-- @since 1.0.0
uniformArray ::
     forall ix e g. (Index ix, RandomGen g, Uniform e)
  => g -- ^ Initial random value generator.
  -> Comp -- ^ Computation strategy.
  -> Sz ix -- ^ Resulting size of the array.
  -> Array DL ix e
uniformArray gen = randomArray gen split uniform

-- | Same as `uniformArray`, but will generate values in a supplied range.
--
-- @since 1.0.0
uniformRangeArray ::
     forall ix e g. (Index ix, RandomGen g, UniformRange e)
  => g -- ^ Initial random value generator.
  -> (e, e) -- ^ Inclusive range in which values will be generated in.
  -> Comp -- ^ Computation strategy.
  -> Sz ix -- ^ Resulting size of the array.
  -> Array DL ix e
uniformRangeArray gen r = randomArray gen split (uniformR r)


-- | Similar to `randomArray` but performs generation sequentially, which means it doesn't
-- require splitability property. Another consequence is that it returns the new generator
-- together with /manifest/ array of random values.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> import System.Random.SplitMix as SplitMix
-- >>> gen = SplitMix.mkSMGen 217
-- >>> snd $ randomArrayS gen (Sz2 2 3) SplitMix.nextDouble :: Array P Ix2 Double
-- Array P Seq (Sz (2 :. 3))
--   [ [ 0.8878273949359751, 0.11290807610140963, 0.7383156058619669 ]
--   , [ 0.39904053166835896, 0.5617584038393628, 0.16248374266020216 ]
--   ]
--
-- >>> import Data.Massiv.Array
-- >>> import System.Random.Mersenne.Pure64 as MT
-- >>> gen = MT.pureMT 217
-- >>> snd $ randomArrayS gen (Sz2 2 3) MT.randomDouble :: Array P Ix2 Double
-- Array P Seq (Sz (2 :. 3))
--   [ [ 0.5504018416543631, 0.22504666452851707, 0.4480480867867128 ]
--   , [ 0.7139711572975297, 0.49401087853770953, 0.9397201599368645 ]
--   ]
--
-- >>> import Data.Massiv.Array
-- >>> import System.Random as System
-- >>> gen = System.mkStdGen 217
-- >>> snd $ randomArrayS gen (Sz2 2 3) System.random :: Array P Ix2 Double
-- Array P Seq (Sz (2 :. 3))
--   [ [ 0.11217260506402493, 0.8870919238985904, 0.2616843941380331 ]
--   , [ 0.600959468331641, 0.4382415961606372, 0.8375162573397977 ]
--   ]
--
-- @since 0.3.4
randomArrayS ::
     forall r ix e g. (Mutable r e, Index ix)
  => g -- ^ Initial random value generator
  -> Sz ix -- ^ Resulting size of the array.
  -> (g -> (e, g))
     -- ^ A function that produces a random value and the next generator
  -> (g, Array r ix e)
randomArrayS gen sz nextRandom =
  runST $ unfoldrPrimM sz (pure . nextRandom) gen
{-# INLINE randomArrayS #-}

-- | This is a stateful approach of generating random values. If your generator is pure
-- and splittable, it is better to use `randomArray` instead, which will give you a pure,
-- deterministic and parallelizable generation of arrays. On the other hand, if your
-- generator is not thread safe, which is most likely the case, instead of using some sort
-- of global mutex, `WorkerStates` allows you to keep track of individual state per worker
-- (thread), which fits parallelization of random value generation perfectly. All that
-- needs to be done is generators need to be initialized once per worker and then they can
-- be reused as many times as necessary.
--
-- ==== __Examples__
--
-- In the example below we take a stateful random number generator from
-- [wmc-random](https://www.stackage.org/package/mwc-random), which is not thread safe,
-- and safely parallelize it by giving each thread it's own generator. There is a caveat
-- of course, statistical independence will depend on the entropy in your initial seeds,
-- so do not use the example below verbatim, since intiial seeds are sequential numbers.
--
-- >>> import Data.Massiv.Array as A
-- >>> import System.Random.MWC as MWC (initialize)
-- >>> import System.Random.Stateful (uniformRM)
-- >>> import Control.Scheduler (initWorkerStates, getWorkerId)
-- >>> :set -XTypeApplications
-- >>> gens <- initWorkerStates (ParN 3) (MWC.initialize . A.toPrimitiveVector . A.singleton @P @Ix1 . fromIntegral . getWorkerId)
-- >>> randomArrayWS gens (Sz2 2 3) (uniformRM (0, 9)) :: IO (Array P Ix2 Double)
-- Array P (ParN 3) (Sz (2 :. 3))
--   [ [ 2.5438514691269685, 4.287612444807011, 5.610339021582389 ]
--   , [ 4.697970155404468, 5.00119167394813, 2.996037154611197 ]
--   ]
-- >>> randomArrayWS gens (Sz1 10) (uniformRM (0, 9)) :: IO (Vector P Int)
-- Array P (ParN 3) (Sz1 10)
--   [ 0, 9, 3, 2, 2, 7, 6, 7, 7, 5 ]
--
-- @since 0.3.4
randomArrayWS ::
     forall r ix e g m. (Mutable r e, Index ix, MonadUnliftIO m, PrimMonad m)
  => WorkerStates g -- ^ Use `initWorkerStates` to initialize you per thread generators
  -> Sz ix -- ^ Resulting size of the array
  -> (g -> m e) -- ^ Generate the value using the per thread generator.
  -> m (Array r ix e)
randomArrayWS states sz genRandom = generateArrayLinearWS states sz (const genRandom)
{-# INLINE randomArrayWS #-}

infix 4 ..., ..:

-- | Handy synonym for @`rangeInclusive` `Seq`@. Similar to @..@ for list.
--
-- >>> Ix1 4 ... 10
-- Array D Seq (Sz1 7)
--   [ 4, 5, 6, 7, 8, 9, 10 ]
--
-- @since 0.3.0
(...) :: Index ix => ix -> ix -> Array D ix ix
(...) = rangeInclusive Seq
{-# INLINE (...) #-}

-- | Handy synonym for @`range` `Seq`@
--
-- >>> Ix1 4 ..: 10
-- Array D Seq (Sz1 6)
--   [ 4, 5, 6, 7, 8, 9 ]
--
-- @since 0.3.0
(..:) :: Index ix => ix -> ix -> Array D ix ix
(..:) = range Seq
{-# INLINE (..:) #-}


-- prop> range comp from to == rangeStep comp from 1 to
--
-- | Create an array of indices with a range from start to finish (not-including), where indices are
-- incremeted by one.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> range Seq (Ix1 1) 6
-- Array D Seq (Sz1 5)
--   [ 1, 2, 3, 4, 5 ]
-- >>> fromIx2 <$> range Seq (-1) (2 :. 2)
-- Array D Seq (Sz (3 :. 3))
--   [ [ (-1,-1), (-1,0), (-1,1) ]
--   , [ (0,-1), (0,0), (0,1) ]
--   , [ (1,-1), (1,0), (1,1) ]
--   ]
--
-- @since 0.1.0
range :: Index ix => Comp -> ix -> ix -> Array D ix ix
range comp !from !to = rangeSize comp from (Sz (liftIndex2 (-) to from))
{-# INLINE range #-}

-- | Same as `range`, but with a custom step.
--
-- /__Throws Exceptions__/: `IndexZeroException`
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> rangeStepM Seq (Ix1 1) 2 8
-- Array D Seq (Sz1 4)
--   [ 1, 3, 5, 7 ]
-- >>> rangeStepM Seq (Ix1 1) 0 8
-- *** Exception: IndexZeroException: 0
--
-- @since 0.3.0
rangeStepM ::
     forall ix m. (Index ix, MonadThrow m)
  => Comp -- ^ Computation strategy
  -> ix -- ^ Start
  -> ix -- ^ Step (Can't have zeros)
  -> ix -- ^ End
  -> m (Array D ix ix)
rangeStepM comp !from !step !to
  | foldlIndex (\acc i -> acc || i == 0) False step = throwM $ IndexZeroException step
  | otherwise =
    let dist = liftIndex2 (-) to from
        sz = liftIndex2 div dist step
        r = liftIndex signum $ liftIndex2 mod dist step
     in pure $ rangeStepSize comp from step (Sz (liftIndex2 (+) sz r))
{-# INLINE rangeStepM #-}

-- | Same as `rangeStepM`, but will throw an error whenever @step@ contains zeros.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> rangeStep' Seq (Ix1 1) 2 6
-- Array D Seq (Sz1 3)
--   [ 1, 3, 5 ]
--
-- @since 0.3.0
rangeStep' :: (HasCallStack, Index ix) => Comp -> ix -> ix -> ix -> Array D ix ix
rangeStep' comp from step = throwEither . rangeStepM comp from step
{-# INLINE rangeStep' #-}

-- | Just like `range`, except the finish index is included.
--
-- @since 0.3.0
rangeInclusive :: Index ix => Comp -> ix -> ix -> Array D ix ix
rangeInclusive comp ixFrom ixTo =
  rangeSize comp ixFrom (Sz (liftIndex2 (-) (liftIndex (+ 1) ixTo) ixFrom))
{-# INLINE rangeInclusive #-}


-- | Just like `rangeStep`, except the finish index is included.
--
-- @since 0.3.0
rangeStepInclusiveM :: (MonadThrow m, Index ix) => Comp -> ix -> ix -> ix -> m (Array D ix ix)
rangeStepInclusiveM comp ixFrom step ixTo = rangeStepM comp ixFrom step (liftIndex (1 +) ixTo)
{-# INLINE rangeStepInclusiveM #-}

-- | Just like `range`, except the finish index is included.
--
-- @since 0.3.1
rangeStepInclusive' :: (HasCallStack, Index ix) => Comp -> ix -> ix -> ix -> Array D ix ix
rangeStepInclusive' comp ixFrom step = throwEither . rangeStepInclusiveM comp ixFrom step
{-# INLINE rangeStepInclusive' #-}


-- | Create an array of specified size with indices starting with some index at position @0@ and
-- incremented by @1@ until the end of the array is reached
--
-- @since 0.3.0
rangeSize :: Index ix =>
             Comp -- ^ Computation strategy
          -> ix -- ^ @x@ - start value
          -> Sz ix -- ^ @sz@ - Size of resulting array
          -> Array D ix ix
rangeSize comp !from !sz = makeArray comp sz (liftIndex2 (+) from)
{-# INLINE rangeSize #-}

-- | Same as `rangeSize`, but with ability to specify the step.
--
-- @since 0.3.0
rangeStepSize :: Index ix =>
                 Comp -- ^ Computation strategy
              -> ix -- ^ @x@ - start value
              -> ix -- ^ @delta@ - step value
              -> Sz ix -- ^ @sz@ - Size of resulting array
              -> Array D ix ix
rangeStepSize comp !from !step !sz =
  makeArray comp sz (liftIndex2 (+) from . liftIndex2 (*) step)
{-# INLINE rangeStepSize #-}


-- | Same as `enumFromStepN` with step @dx = 1@.
--
-- /Related/: `Data.Massiv.Vector.senumFromN`, `Data.Massiv.Vector.senumFromStepN`,
-- `enumFromStepN`, `rangeSize`, `rangeStepSize`, `range`
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> enumFromN Seq (5 :: Double) 3
-- Array D Seq (Sz1 3)
--   [ 5.0, 6.0, 7.0 ]
--
-- __/Similar/__:
--
-- [@Prelude.`Prelude.enumFromTo`@] Very similar to @[i .. i + n - 1]@, except that
-- `senumFromN` is faster, but it only works for `Num` and not for `Enum` elements
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.enumFromN`@]
--
-- @since 0.1.0
enumFromN :: Num e =>
             Comp
          -> e -- ^ @x@ - start value
          -> Sz1 -- ^ @n@ - length of resulting vector.
          -> Vector D e
enumFromN comp !from !sz = makeArrayLinear comp sz $ \ i -> from + fromIntegral i
{-# INLINE enumFromN #-}


-- | Enumerate from a starting number @x@ exactly @n@ times with a custom step value
-- @dx@. Unlike `Data.Massiv.Vector.senumFromStepN`, there is no dependency on neigboring
-- elements therefore `enumFromStepN` is parallelizable.
--
-- /Related/: `Data.Massiv.Vector.senumFromN`, `Data.Massiv.Vector.senumFromStepN`,
-- `enumFromN`, `rangeSize`, `rangeStepSize`, `range`, `rangeStepM`
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> enumFromStepN Seq 1 (0.1 :: Double) 5
-- Array D Seq (Sz1 5)
--   [ 1.0, 1.1, 1.2, 1.3, 1.4 ]
-- >>> enumFromStepN Seq (-pi :: Float) (pi/4) 9
-- Array D Seq (Sz1 9)
--   [ -3.1415927, -2.3561945, -1.5707964, -0.78539824, 0.0, 0.78539824, 1.5707963, 2.3561947, 3.1415927 ]
--
-- __/Similar/__:
--
-- [@Prelude.`Prelude.enumFrom`@] Similar to @take n [x, x + dx ..]@, except that
-- `enumFromStepN` is parallelizable and it only works for `Num` and not for `Enum`
-- elements. Floating point value will be slightly different as well.
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.enumFromStepN`@] Similar in the
-- outcome, but very different in the way it works.
--
--
-- @since 0.1.0
enumFromStepN :: Num e =>
                 Comp
              -> e -- ^ @x@ - start number
              -> e -- ^ @dx@ - step number
              -> Sz1 -- ^ @n@ - length of resulting vector
              -> Vector D e
enumFromStepN comp !from !step !sz = makeArrayLinear comp sz $ \ i -> from + fromIntegral i * step
{-# INLINE enumFromStepN #-}


-- | Function that expands an array to one with a higher dimension.
--
-- This is useful for constructing arrays where there is shared computation
-- between multiple cells.  The makeArray method of constructing arrays:
--
-- > makeArray :: Construct r ix e => Comp -> ix -> (ix -> e) -> Array r ix e
--
-- ...runs a function @ix -> e@ at every array index. This is inefficient if
-- there is a substantial amount of repeated computation that could be shared
-- while constructing elements on the same dimension. The expand functions make
-- this possible. First you construct an @Array r (Lower ix) a@ of one fewer
-- dimensions where @a@ is something like @`Array` r `Ix1` a@ or @`Array` r `Ix2` a@. Then
-- you use 'expandWithin' and a creation function @a -> Int -> b@ to create an
-- @`Array` `D` `Ix2` b@ or @`Array` `D` `Ix3` b@ respectfully.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> a = makeArrayR U Seq (Sz1 6) (+10) -- Imagine (+10) is some expensive function
-- >>> a
-- Array U Seq (Sz1 6)
--   [ 10, 11, 12, 13, 14, 15 ]
-- >>> expandWithin Dim1 5 (\ e j -> (j + 1) * 100 + e) a :: Array D Ix2 Int
-- Array D Seq (Sz (6 :. 5))
--   [ [ 110, 210, 310, 410, 510 ]
--   , [ 111, 211, 311, 411, 511 ]
--   , [ 112, 212, 312, 412, 512 ]
--   , [ 113, 213, 313, 413, 513 ]
--   , [ 114, 214, 314, 414, 514 ]
--   , [ 115, 215, 315, 415, 515 ]
--   ]
-- >>> expandWithin Dim2 5 (\ e j -> (j + 1) * 100 + e) a :: Array D Ix2 Int
-- Array D Seq (Sz (5 :. 6))
--   [ [ 110, 111, 112, 113, 114, 115 ]
--   , [ 210, 211, 212, 213, 214, 215 ]
--   , [ 310, 311, 312, 313, 314, 315 ]
--   , [ 410, 411, 412, 413, 414, 415 ]
--   , [ 510, 511, 512, 513, 514, 515 ]
--   ]
--
-- @since 0.2.6
expandWithin ::
     forall n ix e r a. (IsIndexDimension ix n, Index (Lower ix), Manifest r a)
  => Dimension n
  -> Sz1
  -> (a -> Ix1 -> e)
  -> Array r (Lower ix) a
  -> Array D ix e
expandWithin dim (Sz k) f arr =
  makeArray (getComp arr) sz $ \ix ->
    let (i, ixl) = pullOutDimension ix dim
     in f (unsafeIndex arr ixl) i
  where
    szl = unSz (size arr)
    sz = SafeSz (insertDimension szl dim k)
{-# INLINE expandWithin #-}

-- | Similar to `expandWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
-- @since 0.2.6
expandWithin' ::
     forall r ix a b. (HasCallStack, Index ix, Index (Lower ix), Manifest r a)
  => Dim
  -> Sz1
  -> (a -> Ix1 -> b)
  -> Array r (Lower ix) a
  -> Array D ix b
expandWithin' dim k f = throwEither . expandWithinM dim k f
{-# INLINE expandWithin' #-}

-- | Similar to `expandWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
-- @since 0.4.0
expandWithinM ::
     forall r ix a b m. (Index ix, Index (Lower ix), Manifest r a, MonadThrow m)
  => Dim
  -> Sz1
  -> (a -> Ix1 -> b)
  -> Array r (Lower ix) a
  -> m (Array D ix b)
expandWithinM dim k f arr = do
  sz <- insertSzM (size arr) dim k
  pure $
    makeArray (getComp arr) sz $ \ix ->
      let (i, ixl) = pullOutDim' ix dim -- dim has been checked above
       in f (unsafeIndex arr ixl) i
{-# INLINE expandWithinM #-}

-- | Similar to `expandWithin`, except it uses the outermost dimension.
--
-- @since 0.2.6
expandOuter ::
     forall r ix a b. (Index ix, Index (Lower ix), Manifest r a)
  => Sz1
  -> (a -> Ix1 -> b)
  -> Array r (Lower ix) a
  -> Array D ix b
expandOuter k f arr =
  makeArray (getComp arr) sz $ \ix ->
    let (i, ixl) = unconsDim ix
     in f (unsafeIndex arr ixl) i
  where
    szl = size arr
    sz = consSz k szl
{-# INLINE expandOuter #-}

-- | Similar to `expandWithin`, except it uses the innermost dimension.
--
-- @since 0.2.6
expandInner ::
     forall r ix a b. (Index ix, Index (Lower ix), Manifest r a)
  => Sz1
  -> (a -> Ix1 -> b)
  -> Array r (Lower ix) a
  -> Array D ix b
expandInner k f arr =
  makeArray (getComp arr) sz $ \ix ->
    let (ixl, i) = unsnocDim ix
     in f (unsafeIndex arr ixl) i
  where
    szl = size arr
    sz = snocSz szl k
{-# INLINE expandInner #-}
