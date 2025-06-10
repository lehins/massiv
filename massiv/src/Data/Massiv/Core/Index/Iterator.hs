{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Data.Massiv.Core.Index.Iterator
-- Copyright   : (c) Alexey Kuleshevich 2021-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.Index.Iterator (
  Iterator (..),

  -- * Extra iterator functions
  iterTargetAccST,
  iterTargetAccST_,
  iterTargetFullWithStrideAccST,
  iterTargetFullWithStrideAccST_,
  iterTargetST_,
  iterTargetFullWithStrideST_,

  -- * Iterator implementations
  RowMajor (RowMajor),
  defRowMajor,
  RowMajorLinear (RowMajorLinear),
  defRowMajorLinear,
  RowMajorUnbalanced (RowMajorUnbalanced),
  defRowMajorUnbalanced,

  RowMajor' (RowMajor'),
  defRowMajor',
  RowMajorLinear' (RowMajorLinear'),
  defRowMajorLinear',

) where

import GHC.Exts
import Control.Monad
import Control.Monad.ST
import Control.Scheduler
import Data.Massiv.Core.Index.Internal
import Data.Massiv.Core.Index.Stride
import Data.Massiv.Core.Loop
import Data.Massiv.Core.UnboxedLoop

class Iterator it where
  {-# MINIMAL (iterTargetM, iterTargetA_, iterTargetWithStrideAccST, iterTargetWithStrideAccST_) #-}

  -- | Iterate over a target region using linear index with access to the source
  -- index, which adjusted according to the stride. Use `iterTargetM` if you
  -- need an accumulator.
  --
  -- @since 1.0.2
  iterTargetA_
    :: (Index ix, Applicative f)
    => it
    -> Int
    -- ^ Target linear index start
    -> Sz ix
    -- ^ Target size
    -> ix
    -- ^ Source start index
    -> Stride ix
    -- ^ Source stride
    -> (Ix1 -> ix -> f a)
    -- ^ Action that accepts a linear index of the target and multi-dimensional
    -- index of the source.
    -> f ()

  -- | Iterate over a target region using linear index with access to the source
  -- index, which adjusted according to the stride.
  --
  -- @since 1.0.2
  iterTargetM
    :: (Index ix, Monad m)
    => it
    -> Ix1
    -- ^ Target linear index start
    -> Sz ix
    -- ^ Target size
    -> ix
    -- ^ Source start index
    -> Stride ix
    -- ^ Source stride
    -> a
    -- ^ Accumulator
    -> (Ix1 -> ix -> a -> m a)
    -- ^ Action that accepts a linear index of the target,
    -- multi-dimensional index of the source and accumulator
    -> m a

  iterTargetWithStrideAccST
    :: Index ix
    => it
    -> Scheduler s a
    -- ^ Scheduler to use
    -> Ix1
    -- ^ Target linear start index
    -> Sz ix
    -- ^ Target size
    -> ix
    -- ^ Source start index
    -> Stride ix
    -- ^ Source stride
    -> a
    -- ^ Initial accumulator
    -> (a -> ST s (a, a))
    -- ^ Splitting action that produces new accumulators for separate worker threads.
    -> (Ix1 -> ix -> a -> ST s a)
    -- ^ Action
    -> ST s a

  iterTargetWithStrideAccST_
    :: Index ix
    => it
    -> Scheduler s ()
    -- ^ Scheduler to use
    -> Ix1
    -- ^ Target linear start index
    -> Sz ix
    -- ^ Target size
    -> ix
    -- ^ Start
    -> Stride ix
    -- ^ Stride
    -> a
    -- ^ Initial accumulator
    -> (a -> ST s (a, a))
    -- ^ Splitting action that produces new accumulators for separate worker threads.
    -> (Ix1 -> ix -> a -> ST s a)
    -- ^ Action
    -> ST s ()

  -- | Iterate over a region with a monadic action and accumulator.
  --
  -- @since 1.0.2
  iterFullM
    :: (Index ix, Monad m)
    => it
    -> ix
    -- ^ Source start index
    -> Sz ix
    -- ^ Source size
    -> a
    -- ^ Accumulator
    -> (ix -> a -> m a)
    -- ^ Action that accepts a linear index of the target,
    -- multi-dimensional index of the source and accumulator
    -> m a
  iterFullM it start sz acc f =
    iterTargetM it 0 sz start oneStride acc (const f)
  {-# INLINE iterFullM #-}

  -- | Iterate over a region with an applicative action ignoring the result.
  --
  -- @since 1.0.2
  iterFullA_
    :: (Index ix, Applicative f)
    => it
    -> ix
    -- ^ Source start index
    -> Sz ix
    -- ^ Source size
    -> (ix -> f a)
    -- ^ Action that accepts a linear index of the target,
    -- multi-dimensional index of the source and accumulator
    -> f ()
  iterFullA_ it start sz f =
    iterTargetA_ it 0 sz start oneStride (const f)
  {-# INLINE iterFullA_ #-}

  -- | Iterate over a region in a ST monad with access to `Scheduler`.
  iterFullAccST
    :: Index ix
    => it
    -- ^ Scheduler multiplying factor. Must be positive
    -> Scheduler s a
    -- ^ Scheduler to use
    -> ix
    -- ^ Start index
    -> Sz ix
    -- ^ Size
    -> a
    -- ^ Initial accumulator
    -> (a -> ST s (a, a))
    -- ^ Function that splits accumulator for each scheduled job.
    -> (ix -> a -> ST s a)
    -- ^ Action
    -> ST s a
  iterFullAccST it scheduler start sz acc splitAcc f =
    iterTargetAccST it scheduler 0 sz start acc splitAcc (const f)
  {-# INLINE iterFullAccST #-}

  iterTargetFullAccST
    :: Index ix
    => it
    -> Scheduler s a
    -- ^ Scheduler to use
    -> Ix1
    -- ^ Target linear start index
    -> Sz ix
    -- ^ Target size
    -> a
    -- ^ Initial accumulator
    -> (a -> ST s (a, a))
    -- ^ Function that splits accumulator for each scheduled job.
    -> (Ix1 -> ix -> a -> ST s a)
    -- ^ Action
    -> ST s a
  iterTargetFullAccST it scheduler iStart sz =
    iterTargetFullWithStrideAccST it scheduler iStart sz oneStride
  {-# INLINE iterTargetFullAccST #-}

  iterTargetFullAccST_
    :: Index ix
    => it
    -> Scheduler s ()
    -- ^ Scheduler to use
    -> Ix1
    -- ^ Target linear start index
    -> Sz ix
    -- ^ Target size
    -> a
    -- ^ Initial accumulator
    -> (a -> ST s (a, a))
    -- ^ Function that splits accumulator for each scheduled job.
    -> (Ix1 -> ix -> a -> ST s a)
    -- ^ Action
    -> ST s ()
  iterTargetFullAccST_ it scheduler iStart sz =
    iterTargetFullWithStrideAccST_ it scheduler iStart sz oneStride
  {-# INLINE iterTargetFullAccST_ #-}

  iterTargetFullST_
    :: Index ix
    => it
    -> Scheduler s ()
    -- ^ Scheduler to use
    -> Ix1
    -- ^ Target linear start index
    -> Sz ix
    -- ^ Target size
    -> (Ix1 -> ix -> ST s ())
    -- ^ Action
    -> ST s ()
  iterTargetFullST_ it scheduler iStart sz =
    iterTargetST_ it scheduler iStart sz (pureIndex 0)
  {-# INLINE iterTargetFullST_ #-}

  -- NOTE: this function does not have to be part of the class, but for some
  -- reason it creates a severe regression when moved outside.

  -- | Iterate over a target array with a stride without an accumulator
  iterTargetWithStrideST_
    :: Index ix
    => it
    -> Scheduler s ()
    -- ^ Scheduler to use
    -> Ix1
    -- ^ Target linear start index
    -> Sz ix
    -- ^ Target size
    -> ix
    -- ^ Start
    -> Stride ix
    -- ^ Stride
    -> (Ix1 -> ix -> ST s a)
    -- ^ Action
    -> ST s ()
  iterTargetWithStrideST_ it scheduler i sz ix stride action =
    iterTargetWithStrideAccST_ it scheduler i sz ix stride () noSplit $ \j jx _ ->
      void $ action j jx
  {-# INLINE iterTargetWithStrideST_ #-}

-- | Default iterator that parallelizes work in linear chunks. Supplied factor
-- will be used to schedule that many jobs per capability.
--
-- @since 1.0.2
newtype RowMajor = RowMajorInternal Int

-- | Default row major iterator with multiplying factor set to @8@.
defRowMajor :: RowMajor
defRowMajor = RowMajorInternal 8

pattern RowMajor
  :: Int
  -- ^ Multiplier that will be used to scale number of jobs.
  -> RowMajor
pattern RowMajor f <- RowMajorInternal f
  where
    RowMajor = RowMajorInternal . max 1

{-# COMPLETE RowMajor #-}

instance Iterator RowMajor where
  iterFullM _ start (Sz sz) = iterM start sz (pureIndex 1) (<)
  {-# INLINE iterFullM #-}
  iterFullA_ _ start (Sz sz) = iterA_ start sz (pureIndex 1) (<)
  {-# INLINE iterFullA_ #-}
  iterFullAccST (RowMajorInternal fact) scheduler startIx =
    iterRowMajorST fact scheduler startIx (pureIndex 1)
  {-# INLINE iterFullAccST #-}
  iterTargetA_ _ i sz start (Stride stride) =
    iterTargetRowMajorA_ 0 i sz start stride
  {-# INLINE iterTargetA_ #-}
  iterTargetM _ i sz start (Stride stride) =
    iterTargetRowMajorAccM 0 i sz start stride
  {-# INLINE iterTargetM #-}
  iterTargetWithStrideAccST (RowMajor fact) scheduler i sz ix (Stride stride) =
    iterTargetRowMajorAccST 0 fact scheduler i sz ix stride
  {-# INLINE iterTargetWithStrideAccST #-}
  iterTargetWithStrideAccST_ (RowMajor fact) scheduler i sz ix (Stride stride) =
    iterTargetRowMajorAccST_ 0 fact scheduler i sz ix stride
  {-# INLINE iterTargetWithStrideAccST_ #-}

newtype RowMajorLinear = RowMajorLinear Int

defRowMajorLinear :: RowMajorLinear
defRowMajorLinear = RowMajorLinear 8

instance Iterator RowMajorLinear where
  iterTargetM _ iStart sz start (Stride stride) acc action =
    loopM 0 (< totalElem sz) (+ 1) acc $ \i ->
      action (iStart + i) (liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz i)))
  {-# INLINE iterTargetM #-}
  iterTargetA_ _ iStart sz start (Stride stride) action =
    loopA_ 0 (< totalElem sz) (+ 1) $ \i ->
      action (iStart + i) (liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz i)))
  {-# INLINE iterTargetA_ #-}
  iterTargetFullAccST it scheduler iStart sz acc splitAcc action =
    let !(RowMajorLinear fact) = it
     in iterLinearAccST fact scheduler iStart 1 (totalElem sz) acc splitAcc $ \ !i ->
          action i (fromLinearIndex sz i)
  {-# INLINE iterTargetFullAccST #-}
  iterTargetFullAccST_ it scheduler iStart sz acc splitAcc action =
    let !(RowMajorLinear fact) = it
     in iterLinearAccST_ fact scheduler iStart 1 (totalElem sz) acc splitAcc $ \ !i ->
          action i (fromLinearIndex sz i)
  {-# INLINE iterTargetFullAccST_ #-}
  iterTargetFullST_ it scheduler iStart sz action =
    let !(RowMajorLinear fact) = it
     in iterLinearST_ fact scheduler iStart 1 (totalElem sz) $ \ !i ->
          action i (fromLinearIndex sz i)
  {-# INLINE iterTargetFullST_ #-}
  iterTargetWithStrideAccST it scheduler iStart sz start (Stride stride) acc spliAcc action =
    let RowMajorLinear fact = it
     in iterLinearAccST fact scheduler 0 1 (totalElem sz) acc spliAcc $ \i ->
          action (iStart + i) $
            liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz i))
  {-# INLINE iterTargetWithStrideAccST #-}
  iterTargetWithStrideAccST_ it scheduler iStart sz start (Stride stride) acc spliAcc action =
    let RowMajorLinear fact = it
     in iterLinearAccST_ fact scheduler 0 1 (totalElem sz) acc spliAcc $ \i ->
          action (iStart + i) $
            liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz i))
  {-# INLINE iterTargetWithStrideAccST_ #-}

-- | Parallelizing unbalanced computation (i.e. computing some elements of the
-- array is much more expensive then the others) it can be benefitial to
-- interleave iteration. Perfect example of this would be a ray tracer or the
-- Mandelbrot set.
--
-- iteration without parallelization is equivalent to `RowMajor`
--
-- @since 1.0.2
newtype RowMajorUnbalanced = RowMajorUnbalancedInternal Int

defRowMajorUnbalanced :: RowMajorUnbalanced
defRowMajorUnbalanced = RowMajorUnbalancedInternal 8

pattern RowMajorUnbalanced
  :: Int
  -- ^ Multiplier that will be used to scale number of jobs.
  -> RowMajorUnbalanced
pattern RowMajorUnbalanced f <- RowMajorUnbalancedInternal f
  where
    RowMajorUnbalanced = RowMajorUnbalancedInternal . max 1

{-# COMPLETE RowMajorUnbalanced #-}

instance Iterator RowMajorUnbalanced where
  iterFullM (RowMajorUnbalanced fact) = iterFullM (RowMajor fact)
  {-# INLINE iterFullM #-}
  iterFullA_ (RowMajorUnbalanced fact) = iterFullA_ (RowMajor fact)
  {-# INLINE iterFullA_ #-}
  iterTargetM (RowMajorUnbalanced fact) = iterTargetM (RowMajor fact)
  {-# INLINE iterTargetM #-}
  iterTargetA_ (RowMajorUnbalanced fact) = iterTargetA_ (RowMajor fact)
  {-# INLINE iterTargetA_ #-}
  iterTargetWithStrideAccST = iterUnbalancedTargetWithStride loopM
  {-# INLINE iterTargetWithStrideAccST #-}
  iterTargetWithStrideAccST_ it scheduler iStart sz start stride acc splitAcc' action =
    void $
      iterUnbalancedTargetWithStride innerLoop it scheduler iStart sz start stride acc splitAcc' action
    where
      innerLoop initial condition increment initAcc f =
        void $ loopM initial condition increment initAcc f
      {-# INLINE innerLoop #-}
  {-# INLINE iterTargetWithStrideAccST_ #-}

iterUnbalancedTargetWithStride
  :: Index ix
  => (Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> t) -> ST s b)
  -> RowMajorUnbalanced
  -> Scheduler s b
  -> Int
  -> Sz ix
  -> ix
  -> Stride ix
  -> a
  -> (a -> ST s (a, a))
  -> (Int -> ix -> t)
  -> ST s a
iterUnbalancedTargetWithStride innerLoop it scheduler iStart sz start stride acc splitAcc action =
  let RowMajorUnbalanced fact = it
      !n = totalElem sz
      !step = min (fact * numWorkers scheduler) n
   in loopM 0 (< step) (+ 1) acc $ \ !istep !a -> do
        (curAcc, nextAcc) <- splitAcc a
        scheduleMassivWork scheduler $
          innerLoop istep (< n) (+ step) curAcc $ \i ->
            action (iStart + i) $
              liftIndex2 (+) start (liftIndex2 (*) (unStride stride) (fromLinearIndex sz i))
        pure nextAcc
{-# INLINE iterUnbalancedTargetWithStride #-}

noSplit :: Applicative m => () -> m ((), ())
noSplit _ = pure ((), ())

iterTargetAccST
  :: (Iterator it, Index ix)
  => it
  -> Scheduler s a
  -- ^ Scheduler to use
  -> Ix1
  -- ^ Target linear start index
  -> Sz ix
  -- ^ Target size
  -> ix
  -- ^ Source start
  -> a
  -> (a -> ST s (a, a))
  -> (Ix1 -> ix -> a -> ST s a)
  -- ^ Action
  -> ST s a
iterTargetAccST it scheduler iStart sz ix =
  iterTargetWithStrideAccST it scheduler iStart sz ix oneStride
{-# INLINE iterTargetAccST #-}

iterTargetAccST_
  :: (Iterator it, Index ix)
  => it
  -> Scheduler s ()
  -- ^ Scheduler to use
  -> Ix1
  -- ^ Target linear start index
  -> Sz ix
  -- ^ Target size
  -> ix
  -- ^ Source start
  -> a
  -> (a -> ST s (a, a))
  -> (Ix1 -> ix -> a -> ST s a)
  -- ^ Action
  -> ST s ()
iterTargetAccST_ it scheduler iStart sz ix =
  iterTargetWithStrideAccST_ it scheduler iStart sz ix oneStride
{-# INLINE iterTargetAccST_ #-}

iterTargetFullWithStrideST_
  :: (Iterator it, Index ix)
  => it
  -> Scheduler s ()
  -- ^ Scheduler to use
  -> Ix1
  -- ^ Target linear start index
  -> Sz ix
  -- ^ Target size
  -> Stride ix
  -- ^ Stride
  -> (Ix1 -> ix -> ST s ())
  -- ^ Action
  -> ST s ()
iterTargetFullWithStrideST_ it scheduler iStart sz =
  iterTargetWithStrideST_ it scheduler iStart sz (pureIndex 0)
{-# INLINE iterTargetFullWithStrideST_ #-}

iterTargetST_
  :: (Iterator it, Index ix)
  => it
  -> Scheduler s ()
  -- ^ Scheduler to use
  -> Ix1
  -- ^ Target linear start index
  -> Sz ix
  -- ^ Target size
  -> ix
  -- ^ Start
  -> (Ix1 -> ix -> ST s ())
  -- ^ Action
  -> ST s ()
iterTargetST_ it scheduler iStart sz ix =
  iterTargetWithStrideST_ it scheduler iStart sz ix oneStride
{-# INLINE iterTargetST_ #-}

iterTargetFullWithStrideAccST
  :: (Iterator it, Index ix)
  => it
  -> Scheduler s a
  -- ^ Scheduler to use
  -> Ix1
  -- ^ Target linear start index
  -> Sz ix
  -- ^ Target size
  -> Stride ix
  -- ^ Stride
  -> a
  -> (a -> ST s (a, a))
  -> (Ix1 -> ix -> a -> ST s a)
  -- ^ Action
  -> ST s a
iterTargetFullWithStrideAccST it scheduler iStart sz =
  iterTargetWithStrideAccST it scheduler iStart sz (pureIndex 0)
{-# INLINE iterTargetFullWithStrideAccST #-}

iterTargetFullWithStrideAccST_
  :: (Iterator it, Index ix)
  => it
  -> Scheduler s ()
  -- ^ Scheduler to use
  -> Ix1
  -- ^ Target linear start index
  -> Sz ix
  -- ^ Target size
  -> Stride ix
  -- ^ Stride
  -> a
  -> (a -> ST s (a, a))
  -> (Ix1 -> ix -> a -> ST s a)
  -- ^ Action
  -> ST s ()
iterTargetFullWithStrideAccST_ it scheduler iStart sz =
  iterTargetWithStrideAccST_ it scheduler iStart sz (pureIndex 0)
{-# INLINE iterTargetFullWithStrideAccST_ #-}




---- Experiment


-- | Default iterator that parallelizes work in linear chunks. Supplied factor
-- will be used to schedule that many jobs per capability.
--
-- @since 1.0.2
newtype RowMajor' = RowMajorInternal' Int

-- | Default row major iterator with multiplying factor set to @8@.
defRowMajor' :: RowMajor'
defRowMajor' = RowMajorInternal' 8

pattern RowMajor'
  :: Int
  -- ^ Multiplier that will be used to scale number of jobs.
  -> RowMajor'
pattern RowMajor' f <- RowMajorInternal' f
  where
    RowMajor' = RowMajorInternal' . max 1

{-# COMPLETE RowMajor' #-}

instance Iterator RowMajor' where
  iterFullM _ start (Sz sz) = iterM# start sz (pureIndex 1) (<#)
  {-# INLINE iterFullM #-}
  iterFullA_ _ start (Sz sz) = iterA_# start sz (pureIndex 1) (<#)
  {-# INLINE iterFullA_ #-}
  iterFullAccST (RowMajorInternal' fact) scheduler startIx =
    iterRowMajorST fact scheduler startIx (pureIndex 1)
  {-# INLINE iterFullAccST #-}
  iterTargetA_ _ i sz start (Stride stride) =
    iterTargetRowMajorA_ 0 i sz start stride
  {-# INLINE iterTargetA_ #-}
  iterTargetM _ i sz start (Stride stride) =
    iterTargetRowMajorAccM 0 i sz start stride
  {-# INLINE iterTargetM #-}
  iterTargetWithStrideAccST (RowMajor' fact) scheduler i sz ix (Stride stride) =
    iterTargetRowMajorAccST 0 fact scheduler i sz ix stride
  {-# INLINE iterTargetWithStrideAccST #-}
  iterTargetWithStrideAccST_ (RowMajor' fact) scheduler i sz ix (Stride stride) =
    iterTargetRowMajorAccST_ 0 fact scheduler i sz ix stride
  {-# INLINE iterTargetWithStrideAccST_ #-}


newtype RowMajorLinear' = RowMajorLinear' Int

defRowMajorLinear' :: RowMajorLinear'
defRowMajorLinear' = RowMajorLinear' 8

unI# :: Int -> Int#
unI# (I# i) = i
{-# INLINE unI# #-}

instance Iterator RowMajorLinear' where
  iterTargetM _ iStart sz start (Stride stride) acc action =
    loopM# 0# (<# (unI# (totalElem sz))) (+# 1#) acc $ \i ->
      action (iStart + I# i) (liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz (I# i))))
  {-# INLINE iterTargetM #-}
  iterTargetA_ _ iStart sz start (Stride stride) action =
    loopA_# 0# (<# unI# (totalElem sz)) (+# 1#) $ \i ->
      action (iStart + I# i) (liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz (I# i))))
  {-# INLINE iterTargetA_ #-}
  iterTargetFullAccST it scheduler iStart sz acc splitAcc action =
    let !(RowMajorLinear' fact) = it
     in iterLinearAccST fact scheduler iStart 1 (totalElem sz) acc splitAcc $ \ !i ->
          action i (fromLinearIndex sz i)
  {-# INLINE iterTargetFullAccST #-}
  iterTargetFullAccST_ it scheduler iStart sz acc splitAcc action =
    let !(RowMajorLinear' fact) = it
     in iterLinearAccST_ fact scheduler iStart 1 (totalElem sz) acc splitAcc $ \ !i ->
          action i (fromLinearIndex sz i)
  {-# INLINE iterTargetFullAccST_ #-}
  iterTargetFullST_ it scheduler iStart sz action =
    let !(RowMajorLinear' fact) = it
     in iterLinearST_ fact scheduler iStart 1 (totalElem sz) $ \ !i ->
          action i (fromLinearIndex sz i)
  {-# INLINE iterTargetFullST_ #-}
  iterTargetWithStrideAccST it scheduler iStart sz start (Stride stride) acc spliAcc action =
    let RowMajorLinear' fact = it
     in iterLinearAccST fact scheduler 0 1 (totalElem sz) acc spliAcc $ \i ->
          action (iStart + i) $
            liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz i))
  {-# INLINE iterTargetWithStrideAccST #-}
  iterTargetWithStrideAccST_ it scheduler iStart sz start (Stride stride) acc spliAcc action =
    let RowMajorLinear' fact = it
     in iterLinearAccST_ fact scheduler 0 1 (totalElem sz) acc spliAcc $ \i ->
          action (iStart + i) $
            liftIndex2 (+) start (liftIndex2 (*) stride (fromLinearIndex sz i))
  {-# INLINE iterTargetWithStrideAccST_ #-}
