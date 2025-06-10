{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Data.Massiv.Core.Loop
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.UnboxedLoop (
  Bool#,
  loop#,
  loopF#,
  nextMaybeF#,
  loopA#,
  loopA_#,
  loopM#,
  iloopM#,
  iloopA_#,
  loopNextM#,
  loopNextA_#,
  loopDeepM#,
  splitLinearly#,
  splitLinearlyM#,
  splitLinearlyM_#,
  splitLinearlyWith_#,
  splitLinearlyWithM_#,
  splitLinearlyWithStartAtM_#,
  splitLinearlyWithStatefulM_#,
  iterLinearST_#,
  iterLinearAccST_#,
  iterLinearAccST#,
  splitNumChunks#,
  splitWorkWithFactorST#,
) where

import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Primitive
import Control.Monad.ST (ST)
import Control.Scheduler (
  Scheduler,
  SchedulerWS,
  numWorkers,
  scheduleWork,
  scheduleWorkState_,
  scheduleWork_,
  unwrapSchedulerWS,
 )
import Data.Coerce
import Data.Functor.Identity
import Data.Massiv.Core.Loop (scheduleMassivWork)
import GHC.Exts

type Bool# = Int#

-- | Efficient loop with an accumulator
--
-- @since 0.1.0
loop# :: Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> a -> (Int# -> a -> a) -> a
loop# initial condition increment initAcc f =
  runIdentity (loopM# initial condition increment initAcc (coerce f))
{-# INLINE loop# #-}

-- | Efficient monadic loop with an accumulator
--
-- >>> loopM 1 (< 20) (+ 2) [] (\i a -> Just (i:a))
-- Just [19,17,15,13,11,9,7,5,3,1]
--
-- @since 0.1.0
loopM# :: Monad m => Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> a -> (Int# -> a -> m a) -> m a
loopM# initial condition increment !initAcc f =
  go initial initAcc
  where
    go step !acc =
      case condition step of
        0# -> pure acc
        _ -> f step acc >>= go (increment step)
{-# INLINE loopM# #-}

-- | Efficient monadic loop with an accumulator and extra linear index incremented by 1.
--
-- >>> iloopM 100 1 (< 20) (+ 2) [] (\i ix a -> Just ((i, ix) : a))
-- Just [(109,19),(108,17),(107,15),(106,13),(105,11),(104,9),(103,7),(102,5),(101,3),(100,1)]
--
-- @since 1.0.2
iloopM#
  :: Monad m
  => Int#
  -> Int#
  -> (Int# -> Bool#)
  -> (Int# -> Int#)
  -> a
  -> (Int# -> Int# -> a -> m a)
  -> m a
iloopM# istart initIx condition increment !initAcc f = go istart initIx initAcc
  where
    go i step !acc =
      case condition step of
        0# -> pure acc
        _ -> f i step acc >>= go (i +# 1#) (increment step)
{-# INLINE iloopM# #-}

-- | Efficient monadic loop with extra linear index incremented by 1.
--
-- >>> iloopA_ 100 1 (< 10) (+ 2) (\i ix -> print (i, ix))
-- (100,1)
-- (101,3)
-- (102,5)
-- (103,7)
-- (104,9)
--
-- @since 1.0.2
iloopA_#
  :: Applicative f
  => Int#
  -> Int#
  -> (Int# -> Bool#)
  -> (Int# -> Int#)
  -> (Int# -> Int# -> f a)
  -> f ()
iloopA_# istart initIx condition increment f = go istart initIx
  where
    go i step =
      case condition step of
        0# -> pure ()
        _ -> f i step *> go (i +# 1#) (increment step)
{-# INLINE iloopA_# #-}

-- | Similar to `loopM_` except the action accepts not only the value for current step,
-- but also for the next one as well.
--
-- @since 1.0.2
loopNextA_#
  :: Applicative f => Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> (Int# -> Int# -> f a) -> f ()
loopNextA_# initial condition increment f = go initial
  where
    go step =
      case condition step of
        0# -> pure ()
        _ ->
          case increment step of
            next -> f step next *> go next
{-# INLINE loopNextA_# #-}

-- | Similar to `loopM_` except the action accepts not only the value for current step,
-- but also for the next one as well.
--
-- @since 1.0.2
loopNextM#
  :: Monad m => Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> a -> (Int# -> Int# -> a -> m a) -> m a
loopNextM# initial condition increment !initAcc f = go initial initAcc
  where
    go step !acc =
      case condition step of
        0# -> pure acc
        _ ->
          case increment step of
            next -> f step next acc >>= go next
{-# INLINE loopNextM# #-}

-- | Efficient Applicative loop. Result of each iteration is discarded.
--
-- > loopA_ initial cond incr f === loopA initial cond incr (pure ()) (\i -> id <$ f i)
--
-- @since 1.0.2
loopA_# :: Applicative f => Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> (Int# -> f a) -> f ()
loopA_# initial condition increment f =
  loopF# initial condition increment (pure ()) (\i ma -> f i *> ma)
{-# INLINE loopA_# #-}

-- | Applicative loop. Use monadic `loopM` when possible, since it will be more efficient.
--
-- @since 0.3.0
loopA#
  :: Applicative f => Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> f b -> (Int# -> f (b -> b)) -> f b
loopA# initial condition increment lastAction f =
  loopF# initial condition increment lastAction (\i ma -> f i <*> ma)
{-# INLINE loopA# #-}

loopF# :: Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> f a -> (Int# -> f a -> f a) -> f a
loopF# initial condition increment lastAction f = go initial
  where
    go step =
      case condition step of
        0# -> lastAction
        _ -> f step (go (increment step))
{-# INLINE loopF# #-}

nextMaybeF# :: Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> (Maybe Int -> f a) -> f a
nextMaybeF# cur condition increment f =
  case increment cur of
    i ->
      f $ case condition i of
        0# -> Just (I# i) -- TODO, switch to unboxed sum
        _ -> Nothing
{-# INLINE nextMaybeF# #-}

-- | Similar to `loopM`, but way less efficient monadic loop with an accumulator that reverses
-- the direction of action application. eg:
--
-- >>> loopDeepM 1 (< 20) (+ 2) [] (\i a -> Just (i:a))
-- Just [1,3,5,7,9,11,13,15,17,19]
--
-- Equivalent to:
--
-- >>> loopM 19 (>= 1) (subtract 2) [] (\i a -> Just (i:a))
-- Just [1,3,5,7,9,11,13,15,17,19]
--
-- @since 0.1.0
loopDeepM# :: Monad m => Int# -> (Int# -> Bool#) -> (Int# -> Int#) -> a -> (Int# -> a -> m a) -> m a
loopDeepM# !initial condition increment !initAcc f =
  loopF# initial condition increment (pure initAcc) (\i ma -> ma >>= f i)
{-# INLINE loopDeepM# #-}

-- | Divide length in chunks and apply a function to the computed results
--
-- @since 0.2.1
splitLinearly#
  :: Int#
  -- ^ Number of chunks. Zero will result in a segfault
  -> Int#
  -- ^ Total length
  -> (Int# -> Int# -> a)
  -- ^ Function that accepts a chunk length and slack start index
  -> a
splitLinearly# numChunks totalLength action =
  case totalLength `quotInt#` numChunks of
    chunkLength -> action chunkLength (chunkLength *# numChunks)
{-# INLINE splitLinearly# #-}

numWorkers# :: Scheduler s a -> Int#
numWorkers# scheduler =
  case numWorkers scheduler of
    I# n# -> n#
{-# INLINE numWorkers# #-}

-- | Iterator that expects an action that accepts starting linear index as well as the ending
--
-- @since 0.5.7
splitLinearlyM_#
  :: MonadPrimBase s m => Scheduler s () -> Int# -> (Int# -> Int# -> m ()) -> m ()
splitLinearlyM_# = splitLinearlyM#
{-# INLINE splitLinearlyM_# #-}

-- | Iterator that expects an action that accepts starting linear index as well as the ending
--
-- @since 1.0.2
splitLinearlyM#
  :: MonadPrimBase s m => Scheduler s a -> Int# -> (Int# -> Int# -> m a) -> m ()
splitLinearlyM# scheduler totalLength action =
  splitLinearly# (numWorkers# scheduler) totalLength $ \chunkLength slackStart -> do
    loopNextA_# 0# (<# slackStart) (+# chunkLength) $ \start next ->
      scheduleWork scheduler (action start next)
    case slackStart <# totalLength of
      0# -> pure ()
      _ -> scheduleWork scheduler (action slackStart totalLength)
{-# INLINE splitLinearlyM# #-}

-- | Iterator that can be used to split computation amongst different workers. For monadic
-- generator see `splitLinearlyWithM_`.
--
-- @since 0.2.1
splitLinearlyWith_#
  :: MonadPrimBase s m => Scheduler s () -> Int# -> (Int# -> b) -> (Int# -> b -> m ()) -> m ()
splitLinearlyWith_# scheduler totalLength index =
  splitLinearlyWithM_# scheduler totalLength (\i -> pure (index i))
{-# INLINE splitLinearlyWith_# #-}

-- | Iterator that can be used to split computation jobs
--
-- @since 0.2.6
splitLinearlyWithM_#
  :: MonadPrimBase s m => Scheduler s () -> Int# -> (Int# -> m b) -> (Int# -> b -> m c) -> m ()
splitLinearlyWithM_# scheduler totalLength make write =
  splitLinearlyM_# scheduler totalLength go
  where
    go start end = loopA_# start (<# end) (+# 1#) $ \k -> make k >>= write k
    {-# INLINE go #-}
{-# INLINE splitLinearlyWithM_# #-}

-- | Iterator that can be used to split computation jobs
--
-- @since 0.3.0
splitLinearlyWithStartAtM_#
  :: MonadPrimBase s m => Scheduler s () -> Int# -> Int# -> (Int# -> m b) -> (Int# -> b -> m c) -> m ()
splitLinearlyWithStartAtM_# scheduler startAt totalLength make write =
  splitLinearly# (numWorkers# scheduler) totalLength $ \chunkLength slackStart -> do
    loopA_# startAt (<# (slackStart +# startAt)) (+# chunkLength) $ \ !start ->
      scheduleWork_ scheduler $
        loopA_# start (<# (start +# chunkLength)) (+# 1#) $
          \k -> make k >>= write k
    case slackStart <# totalLength of
      0# -> pure ()
      _ ->
        scheduleWork_ scheduler $
          loopA_# (slackStart +# startAt) (<# (totalLength +# startAt)) (+# 1#) $
            \k -> make k >>= write k
{-# INLINE splitLinearlyWithStartAtM_# #-}

-- | Iterator that can be used to split computation jobs, while using a stateful scheduler.
--
-- @since 0.3.4
splitLinearlyWithStatefulM_#
  :: MonadUnliftIO m
  => SchedulerWS ws ()
  -> Int#
  -- ^ Total linear length
  -> (Int# -> ws -> m b)
  -- ^ Element producing action
  -> (Int# -> b -> m c)
  -- ^ Element storing action
  -> m ()
splitLinearlyWithStatefulM_# schedulerWS totalLength make store =
  withRunInIO $ \run ->
    splitLinearly# (numWorkers# (unwrapSchedulerWS schedulerWS)) totalLength $
      \chunkLength slackStart -> do
        loopA_# 0# (<# slackStart) (+# chunkLength) $ \ !start ->
          scheduleWorkState_ schedulerWS $ \s ->
            loopA_# start (<# (start +# chunkLength)) (+# 1#) $ \ !k ->
              run (make k s >>= store k)
        scheduleWorkState_ schedulerWS $ \s ->
          loopA_# slackStart (<# totalLength) (+# 1#) $ \ !k ->
            run (make k s >>= store k)
{-# INLINE splitLinearlyWithStatefulM_# #-}

-- | This is a major helper function for fair splitting and parallelization of
-- work with ability to use some arbitrary accumulator and splittable seed
--
-- @since 1.0.2
splitWorkWithFactorST#
  :: Int#
  -- ^ Multiplying factor to be applied to number of workers for number
  -- of jobs to schedule. Higher the factor, more jobs will be
  -- scheduled. Only positive values are valid.
  -> Scheduler s a
  -> Int#
  -- ^ Starting index
  -> Int#
  -- ^ Stepping value. Can be negative, but must not be zero.
  -> Int#
  -- ^ Total number of steps to be taken
  -> b
  -- ^ Initial value for an accumulator
  -> (b -> ST s (b, b)) -- TODO: switch to unboxed tuples. Will require adjusting unconsSz, etc too

  -- ^ An action to split accumulator for multiple threads
  -> (Int# -> Int# -> Int# -> Int# -> b -> ST s a)
  -- ^ A job to be scheduled. Accepts:
  --
  -- * Chunk index start
  -- * Chunk length
  -- * Chunk start index adjusted for supplied start and stepping value
  -- * Chunk stop index adjusted for supplied start and stepping value
  -> ST s b
splitWorkWithFactorST# fact scheduler start step totalLength initAcc splitAcc action =
  case splitNumChunks# fact (numWorkers# scheduler) totalLength of
    (# chunkLength, slackStart #) -> do
      slackAcc <-
        loopM# 0# (<# slackStart) (+# chunkLength) initAcc $ \chunkStart !acc -> do
          (accCur, accNext) <- splitAcc acc
          scheduleMassivWork scheduler $ do
            case calcRange chunkStart chunkLength of
              (# chunkStartAdj, chunkStopAdj #) ->
                action chunkStart chunkLength chunkStartAdj chunkStopAdj accCur
          pure accNext
      case totalLength -# slackStart of
        slackLength ->
          case slackLength ># 0# of
            0# -> pure slackAcc
            _ -> do
              (curAcc, nextAcc) <- splitAcc slackAcc
              scheduleMassivWork scheduler $
                case calcRange slackStart slackLength of
                  (# slackStartAdj, slackStopAdj #) ->
                    action slackStart slackLength slackStartAdj slackStopAdj curAcc
              pure nextAcc
  where
    calcRange cStart len =
      case start +# cStart *# step of
        cStartAdj -> (# cStartAdj, cStartAdj +# len *# step #)
    {-# INLINE calcRange #-}
{-# INLINE splitWorkWithFactorST# #-}

-- | Linear iterator that supports multiplying factor
--
-- @since 1.0.2
iterLinearST_#
  :: Int#
  -- ^ Zero will result in a segfault
  -> Scheduler s ()
  -> Int#
  -> Int#
  -- ^ Step. Zero will result in a segfault
  -> Int#
  -> (Int# -> ST s a)
  -> ST s ()
iterLinearST_# fact scheduler start step n action =
  case (n -# start) `quotInt#` step of
    totalLength ->
      splitWorkWithFactorST# fact scheduler start step totalLength () (\_ -> pure ((), ())) $
        \_ _ chunkStartAdj chunkStopAdj _ ->
          loopA_# chunkStartAdj (<# chunkStopAdj) (+# step) action
{-# INLINE iterLinearST_# #-}

-- | Linear iterator that supports multiplying factor and accumulator, but the results are discarded.
--
-- @since 1.0.2
iterLinearAccST_#
  :: Int#
  -> Scheduler s ()
  -> Int#
  -> Int#
  -> Int#
  -> a
  -> (a -> ST s (a, a))
  -> (Int# -> a -> ST s a)
  -> ST s ()
iterLinearAccST_# fact scheduler start step n initAcc splitAcc action =
  case (n -# start) `quotInt#` step of
    totalLength ->
      void $
        splitWorkWithFactorST# fact scheduler start step totalLength initAcc splitAcc $
          \_ _ chunkStartAdj chunkStopAdj accCur ->
            void $ loopM# chunkStartAdj (<# chunkStopAdj) (+# step) accCur action
{-# INLINE iterLinearAccST_# #-}

-- | Linear iterator that supports multiplying factor and accumulator. Results
-- of actions are stored in the scheduler.
--
-- @since 1.0.2
iterLinearAccST#
  :: Int#
  -> Scheduler s a
  -> Int#
  -> Int#
  -- ^ Step. Must be non-zero, or segfault
  -> Int#
  -> a
  -> (a -> ST s (a, a))
  -> (Int# -> a -> ST s a)
  -> ST s a
iterLinearAccST# fact scheduler start step n initAcc splitAcc action = do
  case (n -# start) `quotInt#` step of
    totalLength ->
      splitWorkWithFactorST# fact scheduler start step totalLength initAcc splitAcc $
        \_ _ chunkStartAdj chunkStopAdj accCur ->
          loopM# chunkStartAdj (<# chunkStopAdj) (+# step) accCur action
{-# INLINE iterLinearAccST# #-}

-- | Helper for figuring out the chunk length and slack start
splitNumChunks# :: Int# -> Int# -> Int# -> (# Int#, Int# #)
splitNumChunks# fact nw totalLength =
  case ( case nw /=# 1# of -- Optimize for Seq
          0# -> 1#
          _ ->
            case totalLength ># nw of -- Avoid division by zero
              0# -> 1#
              _ ->
                case nw *# fact of
                  maxNumChunks ->
                    case totalLength <# maxNumChunks of
                      0# -> maxNumChunks
                      _ -> nw
       ) of
    numChunks ->
      case totalLength `quotInt#` numChunks of
        chunkLength ->
          (# chunkLength, chunkLength *# numChunks #)
