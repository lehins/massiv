{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Core.Common
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.Common
  ( Array
  , Matrix
  , Vector
  , Elt
  , Steps(..)
  , Stream(..)
  , Construct(..)
  , Source(..)
  , Load(..)
  , StrideLoad(..)
  , Resize(..)
  , Extract(..)
  , Slice(..)
  , OuterSlice(..)
  , InnerSlice(..)
  , Manifest(..)
  , Mutable(..)
  , Comp(..)
  , Scheduler
  , numWorkers
  , scheduleWork
  , scheduleWork_
  , WorkerStates
  , unsafeRead
  , unsafeWrite
  , unsafeModify
  , unsafeLinearModify
  , unsafeSwap
  , unsafeLinearSwap
  , unsafeDefaultLinearShrink
  , Ragged(..)
  , Nested(..)
  , NestedStruct
  , empty
  , singleton
  -- * Size
  , elemsCount
  , isEmpty
  , Sz(SafeSz)
  , Size(..)
  -- * Indexing
  , (!?)
  , index
  , indexM
  , (!)
  , index'
  , (??)
  , defaultIndex
  , borderIndex
  , evaluateM
  , evaluate'
  , module Data.Massiv.Core.Index
  -- * Common Operations
  , imapM_
  , liftArray
  , liftArray2'
  , liftArray2M
  , unsafeLiftArray2
  , Semigroup((<>))
  -- * Exceptions
  , MonadThrow(..)
  , throw
  , IndexException(..)
  , SizeException(..)
  , ShapeException(..)
  , module Data.Massiv.Core.Exception
  , Proxy(..)
  , Id(..)
  -- * Stateful Monads
  , MonadUnliftIO
  , MonadIO(liftIO)
  , PrimMonad(PrimState)
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Control.Exception (throw)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Unlift (MonadIO(liftIO), MonadUnliftIO)
import Control.Monad.Primitive
import Control.Scheduler (Comp(..), Scheduler, WorkerStates, numWorkers,
                          scheduleWork, scheduleWork_, withScheduler_, trivialScheduler_)
import Data.Massiv.Core.Exception
import Data.Massiv.Core.Index
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Data.Typeable
import Data.Vector.Fusion.Bundle.Size
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util

#include "massiv.h"

-- | The array family. Representations @r@ describes how data is arranged or computed. All arrays
-- have a common property that each index @ix@ always maps to the same unique element, even if that
-- element does not exist in memory and has to be computed upon lookup. Data is always arranged in a
-- nested fashion, depth of which is controlled by @`Rank` ix@.
data family Array r ix e :: *

type Matrix r e = Array r Ix2 e

type Vector r e = Array r Ix1 e

type family Elt r ix e :: * where
  Elt r Ix1 e = e
  Elt r ix  e = Array (R r) (Lower ix) e

type family NestedStruct r ix e :: *



class Stream r ix e where
  toStream :: Array r ix e -> Steps Id e

data Steps m e = Steps
  { stepsStream :: S.Stream m e
  , stepsSize   :: Size
  }

instance Monad m => Functor (Steps m) where
  fmap f s = s { stepsStream = S.map f (stepsStream s) }
  {-# INLINE fmap #-}


-- | Array types that can be constructed.
class (Typeable r, Index ix) => Construct r ix e where
  {-# MINIMAL (makeArray|makeArrayLinear) #-}

  -- | Construct an Array. Resulting type either has to be unambiguously inferred or restricted
  -- manually, like in the example below. Use "Data.Massiv.Array.makeArrayR" if you'd like to
  -- specify representation as an argument.
  --
  -- >>> import Data.Massiv.Array
  -- >>> makeArray Seq (Sz (3 :. 4)) (\ (i :. j) -> if i == j then i else 0) :: Array D Ix2 Int
  -- Array D Seq (Sz (3 :. 4))
  --   [ [ 0, 0, 0, 0 ]
  --   , [ 0, 1, 0, 0 ]
  --   , [ 0, 0, 2, 0 ]
  --   ]
  --
  -- Instead of restricting the full type manually we can use `TypeApplications` as convenience:
  --
  -- >>> :set -XTypeApplications
  -- >>> makeArray @P @_ @Double Seq (Sz2 3 4) $ \(i :. j) -> logBase (fromIntegral i) (fromIntegral j)
  -- Array P Seq (Sz (3 :. 4))
  --   [ [ NaN, -0.0, -0.0, -0.0 ]
  --   , [ -Infinity, NaN, Infinity, Infinity ]
  --   , [ -Infinity, 0.0, 1.0, 1.5849625007211563 ]
  --   ]
  --
  -- @since 0.1.0
  makeArray ::
       Comp -- ^ Computation strategy. Useful constructors are `Seq` and `Par`
    -> Sz ix -- ^ Size of the result array.
    -> (ix -> e) -- ^ Function to generate elements at a particular index
    -> Array r ix e
  makeArray comp sz f = makeArrayLinear comp sz (f . fromLinearIndex sz)
  {-# INLINE makeArray #-}

  -- | Same as `makeArray`, but produce elements using linear row-major index.
  --
  -- >>> import Data.Massiv.Array
  -- >>> makeArrayLinear Seq (Sz (2 :. 4)) id :: Array D Ix2 Int
  -- Array D Seq (Sz (2 :. 4))
  --   [ [ 0, 1, 2, 3 ]
  --   , [ 4, 5, 6, 7 ]
  --   ]
  --
  -- @since 0.3.0
  makeArrayLinear :: Comp -> Sz ix -> (Int -> e) -> Array r ix e
  makeArrayLinear comp sz f = makeArray comp sz (f . toLinearIndex sz)
  {-# INLINE makeArrayLinear #-}

  -- | Create an array with the same value in each cell. Array can be created sequentially.
  --
  -- @since 0.4.1
  makeConstantArray :: Sz ix -> e -> Array r ix e
  makeConstantArray sz e = makeArrayLinear Seq sz (const e)
  {-# INLINE makeConstantArray #-}


class Index ix => Resize r ix where
  -- | /O(1)/ - Change the size of an array. Total number of elements should be the same, but it is
  -- not validated.
  unsafeResize :: Index ix' => Sz ix' -> Array r ix e -> Array r ix' e


class Load r ix e => Extract r ix e where
  -- | /O(1)/ - Extract a portion of an array. Staring index and new size are
  -- not validated.
  unsafeExtract :: ix -> Sz ix -> Array r ix e -> Array (R r) ix e


-- | Arrays that can be used as source to practically any manipulation function.
class Load r ix e => Source r ix e where
  {-# MINIMAL (unsafeIndex|unsafeLinearIndex), unsafeLinearSlice #-}

  -- | Lookup element in the array. No bounds check is performed and access of
  -- arbitrary memory is possible when invalid index is supplied.
  --
  -- @since 0.1.0
  unsafeIndex :: Array r ix e -> ix -> e
  unsafeIndex =
    INDEX_CHECK("(Source r ix e).unsafeIndex",
                size, \ !arr -> unsafeLinearIndex arr . toLinearIndex (size arr))
  {-# INLINE unsafeIndex #-}

  -- | Lookup element in the array using flat index in a row-major fashion. No
  -- bounds check is performed
  --
  -- @since 0.1.0
  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}

  -- | Source arrays also give us ability to look at their linear slices
  --
  -- @since 0.4.1
  unsafeLinearSlice :: Ix1 -> Sz1 -> Array r ix e -> Array r Ix1 e

-- | Any array that can be computed and loaded into memory
class (Typeable r, Index ix) => Load r ix e where
  type family R r :: *
  type instance R r = r

  -- | Get computation strategy of this array
  --
  -- @since 0.1.0
  getComp :: Array r ix e -> Comp

  -- | Set computation strategy for this array
  --
  -- ==== __Example__
  --
  -- >>> :set -XTypeApplications
  -- >>> import Data.Massiv.Array
  -- >>> a = singleton @DL @Ix1 @Int 0
  -- >>> a
  -- Array DL Seq (Sz1 1)
  --   [ 0 ]
  -- >>> setComp (ParN 6) a -- use 6 capabilities
  -- Array DL (ParN 6) (Sz1 1)
  --   [ 0 ]
  --
  setComp :: Comp -> Array r ix e -> Array r ix e

  -- | Get the size of an immutabe array
  --
  -- @since 0.1.0
  size :: Array r ix e -> Sz ix


  -- | Load an array into memory.
  --
  -- @since 0.3.0
  loadArrayM
    :: Monad m =>
       Scheduler m ()
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()

  defaultElement :: Array r ix e -> Maybe e
  defaultElement _ = Nothing
  {-# INLINE defaultElement #-}

  -- | /O(1)/ - Get the possible maximum size of an immutabe array. If the lookup of size
  -- in constant time is not possible, `Nothing` should be returned. This value will be
  -- used as the initial size of the mutable array in which loading will happen.
  --
  -- @since 0.4.1
  maxSize :: Array r ix e -> Maybe (Sz ix)
  maxSize = Just . size
  {-# INLINE maxSize #-}

  -- | Load into a supplied mutable array sequentially. Returned array does npt have to be
  -- the same
  --
  -- @since 0.4.1
  unsafeLoadIntoS ::
       (Mutable r' ix e, PrimMonad m)
    => MArray (PrimState m) r' ix e
    -> Array r ix e
    -> m (MArray (PrimState m) r' ix e)
  unsafeLoadIntoS marr arr = do
    loadArrayM trivialScheduler_ arr (unsafeLinearWrite marr)
    pure marr
  {-# INLINE unsafeLoadIntoS #-}

  -- | Same as `unsafeLoadIntoS`, but with respect of computation startegy.
  --
  -- @since 0.4.1
  unsafeLoadInto ::
       (Mutable r' ix e, MonadIO m)
    => MArray RealWorld r' ix e
    -> Array r ix e
    -> m (MArray RealWorld r' ix e)
  unsafeLoadInto marr arr = do
    liftIO $ withScheduler_ (getComp arr) $ \scheduler ->
      loadArrayM scheduler arr (unsafeLinearWrite marr)
    pure marr
  {-# INLINE unsafeLoadInto #-}


class Load r ix e => StrideLoad r ix e where
  -- | Load an array into memory with stride. Default implementation requires an instance of
  -- `Source`.
  loadArrayWithStrideM
    :: Monad m =>
       Scheduler m ()
    -> Stride ix -- ^ Stride to use
    -> Sz ix -- ^ Size of the target array affected by the stride.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()
  default loadArrayWithStrideM
    :: (Source r ix e, Monad m) =>
       Scheduler m ()
    -> Stride ix
    -> Sz ix
    -> Array r ix e
    -> (Int -> e -> m ())
    -> m ()
  loadArrayWithStrideM scheduler stride resultSize arr =
    splitLinearlyWith_ scheduler (totalElem resultSize) unsafeLinearWriteWithStride
    where
      !strideIx = unStride stride
      unsafeLinearWriteWithStride =
        unsafeIndex arr . liftIndex2 (*) strideIx . fromLinearIndex resultSize
      {-# INLINE unsafeLinearWriteWithStride #-}
  {-# INLINE loadArrayWithStrideM #-}


class Load r ix e => OuterSlice r ix e where
  -- | /O(1)/ - Take a slice out of an array from the outside
  unsafeOuterSlice :: Array r ix e -> Int -> Elt r ix e

class Load r ix e => InnerSlice r ix e where
  unsafeInnerSlice :: Array r ix e -> (Sz (Lower ix), Sz Int) -> Int -> Elt r ix e

class Load r ix e => Slice r ix e where
  unsafeSlice :: MonadThrow m => Array r ix e -> ix -> Sz ix -> Dim -> m (Elt r ix e)


-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- @(`!`)@, @(`!?`)@, etc. are constrained to manifest arrays only.
class (Load r ix e, Source r ix e) => Manifest r ix e where

  unsafeLinearIndexM :: Array r ix e -> Int -> e


class (Construct r ix e, Manifest r ix e) => Mutable r ix e where
  data MArray s r ix e :: *

  -- | Get the size of a mutable array.
  --
  -- @since 0.1.0
  msize :: MArray s r ix e -> Sz ix

  -- | Convert immutable array into a mutable array without copy.
  --
  -- @since 0.1.0
  unsafeThaw :: PrimMonad m => Array r ix e -> m (MArray (PrimState m) r ix e)

  -- | Convert mutable array into an immutable array without copy.
  --
  -- @since 0.1.0
  unsafeFreeze :: PrimMonad m => Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)

  -- | Create new mutable array, leaving it's elements uninitialized. Size isn't validated either.
  --
  -- @since 0.1.0
  unsafeNew :: PrimMonad m => Sz ix -> m (MArray (PrimState m) r ix e)

  -- | Read an element at linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearRead :: PrimMonad m => MArray (PrimState m) r ix e -> Int -> m e

  -- | Write an element into mutable array with linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearWrite :: PrimMonad m => MArray (PrimState m) r ix e -> Int -> e -> m ()

  -- | Initialize mutable array to some default value.
  --
  -- @since 0.3.0
  initialize :: PrimMonad m => MArray (PrimState m) r ix e -> m ()

  -- | Create new mutable array while initializing all elements to some default value.
  --
  -- @since 0.3.0
  initializeNew :: PrimMonad m => Maybe e -> Sz ix -> m (MArray (PrimState m) r ix e)
  initializeNew mdef sz = do
    marr <- unsafeNew sz
    case mdef of
      Just val -> unsafeLinearSet marr 0 (SafeSz (totalElem sz)) val
      Nothing  -> initialize marr
    return marr
  {-# INLINE initializeNew #-}

  -- | Set all cells in the mutable array within the range to a specified value.
  --
  -- @since 0.3.0
  unsafeLinearSet :: PrimMonad m =>
                     MArray (PrimState m) r ix e -> Ix1 -> Sz1 -> e -> m ()
  unsafeLinearSet marr offset len e =
    loopM_ offset (< (offset + unSz len)) (+1) (\i -> unsafeLinearWrite marr i e)
  {-# INLINE unsafeLinearSet #-}

  -- | Copy part of one mutable array into another
  --
  -- @since 0.3.6
  unsafeLinearCopy :: (Mutable r ix' e, PrimMonad m) =>
                      MArray (PrimState m) r ix' e -- ^ Source mutable array
                   -> Ix1 -- ^ Starting index at source array
                   -> MArray (PrimState m) r ix e -- ^ Target mutable array
                   -> Ix1 -- ^ Starting index at target array
                   -> Sz1 -- ^ Number of elements to copy
                   -> m ()
  unsafeLinearCopy marrFrom iFrom marrTo iTo (SafeSz k) = do
    let delta = iTo - iFrom
    loopM_ iFrom (< k + iFrom) (+1) $ \i ->
      unsafeLinearRead marrFrom i >>= unsafeLinearWrite marrTo (i + delta)
  {-# INLINE unsafeLinearCopy #-}

  -- | Copy a part of a pure array into a mutable array
  --
  -- @since 0.3.6
  unsafeArrayLinearCopy :: (Mutable r ix' e, PrimMonad m) =>
                           Array r ix' e -- ^ Source pure array
                        -> Ix1 -- ^ Starting index at source array
                        -> MArray (PrimState m) r ix e -- ^ Target mutable array
                        -> Ix1 -- ^ Starting index at target array
                        -> Sz1 -- ^ Number of elements to copy
                        -> m ()
  unsafeArrayLinearCopy arrFrom iFrom marrTo iTo (SafeSz k) = do
    let delta = iTo - iFrom
    loopM_ iFrom (< k + iFrom) (+1) $ \i ->
      unsafeLinearWrite marrTo (i + delta) (unsafeLinearIndex arrFrom i)
  {-# INLINE unsafeArrayLinearCopy #-}

  -- | Linearly reduce the size of an array. Total number of elements should be smaller or
  -- equal. There is no guarantee that the original array is left unchanged, so it should
  -- no longer be used.
  --
  -- @since 0.3.6
  unsafeLinearShrink :: PrimMonad m =>
                        MArray (PrimState m) r ix e -> Sz ix -> m (MArray (PrimState m) r ix e)
  unsafeLinearShrink = unsafeDefaultLinearShrink
  {-# INLINE unsafeLinearShrink #-}

  -- | Linearly increase the size of an array. Total number of elements should be larger
  -- or equal. There is no guarantee that the original array is left unchanged, so it
  -- should no longer be used.
  --
  -- @since 0.3.6
  unsafeLinearGrow :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Sz ix -> m (MArray (PrimState m) r ix e)
  unsafeLinearGrow marr sz = do
    marr' <- unsafeNew sz
    unsafeLinearCopy marr 0 marr' 0 $ SafeSz (totalElem (msize marr))
    pure marr'
  {-# INLINE unsafeLinearGrow #-}


unsafeDefaultLinearShrink ::
     (Mutable r ix e, PrimMonad m)
  => MArray (PrimState m) r ix e
  -> Sz ix
  -> m (MArray (PrimState m) r ix e)
unsafeDefaultLinearShrink marr sz = do
  marr' <- unsafeNew sz
  unsafeLinearCopy marr 0 marr' 0 $ SafeSz (totalElem sz)
  pure marr'
{-# INLINE unsafeDefaultLinearShrink #-}


-- | Read an array element
--
-- @since 0.1.0
unsafeRead :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> m e
unsafeRead !marr !ix = unsafeLinearRead marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeRead #-}

-- | Write an element into array
--
-- @since 0.1.0
unsafeWrite :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> e -> m ()
unsafeWrite !marr !ix = unsafeLinearWrite marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeWrite #-}


-- | Modify an element in the array with a monadic action. Returns the previous value.
--
-- @since 0.4.0
unsafeLinearModify :: (Mutable r ix e, PrimMonad m) =>
                      MArray (PrimState m) r ix e -> (e -> m e) -> Int -> m e
unsafeLinearModify !marr f !i = do
  v <- unsafeLinearRead marr i
  v' <- f v
  unsafeLinearWrite marr i v'
  pure v
{-# INLINE unsafeLinearModify #-}

-- | Modify an element in the array with a monadic action. Returns the previous value.
--
-- @since 0.4.0
unsafeModify :: (Mutable r ix e, PrimMonad m) =>
                MArray (PrimState m) r ix e -> (e -> m e) -> ix -> m e
unsafeModify marr f ix = unsafeLinearModify marr f (toLinearIndex (msize marr) ix)
{-# INLINE unsafeModify #-}

-- | Swap two elements in a mutable array under the supplied indices. Returns the previous
-- values.
--
-- @since 0.4.0
unsafeSwap :: (Mutable r ix e, PrimMonad m) =>
                    MArray (PrimState m) r ix e -> ix -> ix -> m (e, e)
unsafeSwap !marr !ix1 !ix2 = unsafeLinearSwap marr (toLinearIndex sz ix1) (toLinearIndex sz ix2)
  where sz = msize marr
{-# INLINE unsafeSwap #-}


-- | Swap two elements in a mutable array under the supplied linear indices. Returns the
-- previous values.
--
-- @since 0.4.0
unsafeLinearSwap :: (Mutable r ix e, PrimMonad m) =>
                    MArray (PrimState m) r ix e -> Int -> Int -> m (e, e)
unsafeLinearSwap !marr !i1 !i2 = do
  val1 <- unsafeLinearRead marr i1
  val2 <- unsafeLinearRead marr i2
  unsafeLinearWrite marr i1 val2
  unsafeLinearWrite marr i2 val1
  return (val1, val2)
{-# INLINE unsafeLinearSwap #-}


class Nested r ix e where
  fromNested :: NestedStruct r ix e -> Array r ix e

  toNested :: Array r ix e -> NestedStruct r ix e

class Construct r ix e => Ragged r ix e where

  emptyR :: Comp -> Array r ix e

  isNull :: Array r ix e -> Bool

  consR :: Elt r ix e -> Array r ix e -> Array r ix e

  unconsR :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  generateRaggedM :: Monad m => Comp -> Sz ix -> (ix -> m e) -> m (Array r ix e)

  edgeSize :: Array r ix e -> Sz ix

  flattenRagged :: Array r ix e -> Array r Ix1 e

  loadRagged ::
    Monad m => (m () -> m ()) -> (Int -> e -> m a) -> Int -> Int -> Sz ix -> Array r ix e -> m ()

  -- TODO: test property:
  -- (read $ raggedFormat show "\n" (ls :: Array L (IxN n) Int)) == ls
  raggedFormat :: (e -> String) -> String -> Array r ix e -> String



-- | Create an Array with no elements. By itself it is not particularly useful, but it serves as a
-- nice base for constructing larger arrays.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XTypeApplications
-- >>> xs = empty @DL @Ix1 @Double
-- >>> snoc (cons 4 (cons 5 xs)) 22
-- Array DL Seq (Sz1 3)
--   [ 4.0, 5.0, 22.0 ]
--
-- @since 0.3.0
empty ::
     forall r ix e. Construct r ix e
  => Array r ix e
empty = makeArray Seq zeroSz (const (throwImpossible Uninitialized))
{-# INLINE empty #-}

-- | Create an Array with a single element.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> singleton 7 :: Array D Ix4 Double
-- Array D Seq (Sz (1 :> 1 :> 1 :. 1))
--   [ [ [ [ 7.0 ]
--       ]
--     ]
--   ]
--
-- Instead of specifying type signature we could use @TypeApplications@
--
-- >>> :set -XTypeApplications
-- >>> singleton @U @Ix4 @Double 7
-- Array U Seq (Sz (1 :> 1 :> 1 :. 1))
--   [ [ [ [ 7.0 ]
--       ]
--     ]
--   ]
--
-- @since 0.1.0
singleton ::
     forall r ix e. Construct r ix e
  => e -- ^ The only element
  -> Array r ix e
singleton = makeArray Seq oneSz . const
{-# INLINE singleton #-}


infixl 4 !, !?, ??

-- | Infix version of `index'`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs U $ iterateN (Sz (2 :. 3)) succ (0 :: Int)
-- >>> a
-- Array U Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
-- >>> a ! 0 :. 2
-- 3
-- >>> a ! 0 :. 3
-- *** Exception: IndexOutOfBoundsException: (0 :. 3) is not safe for (Sz (2 :. 3))
--
-- @since 0.1.0
(!) :: Manifest r ix e => Array r ix e -> ix -> e
(!) = index'
{-# INLINE (!) #-}


-- | Infix version of `indexM`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XTypeApplications
-- >>> a <- fromListsM @U @Ix2 @Int Seq [[1,2,3],[4,5,6]]
-- >>> a
-- Array U Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
-- >>> a !? 0 :. 2
-- 3
-- >>> a !? 0 :. 3
-- *** Exception: IndexOutOfBoundsException: (0 :. 3) is not safe for (Sz (2 :. 3))
-- >>> a !? 0 :. 3 :: Maybe Int
-- Nothing
--
-- @since 0.1.0
(!?) :: (Manifest r ix e, MonadThrow m) => Array r ix e -> ix -> m e
(!?) = indexM
{-# INLINE (!?) #-}


-- | /O(1)/ - Lookup an element in the array, where array itself is wrapped with
-- `MonadThrow`. This operator is useful when used together with slicing or other
-- functions that can fail.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XTypeApplications
-- >>> ma = fromListsM @U @Ix3 @Int @Maybe Seq [[[1,2,3]],[[4,5,6]]]
-- >>> ma
-- Just (Array U Seq (Sz (2 :> 1 :. 3))
--   [ [ [ 1, 2, 3 ]
--     ]
--   , [ [ 4, 5, 6 ]
--     ]
--   ]
-- )
-- >>> ma ??> 1
-- Just (Array M Seq (Sz (1 :. 3))
--   [ [ 4, 5, 6 ]
--   ]
-- )
-- >>> ma ??> 1 ?? 0 :. 2
-- Just 6
-- >>> ma ?? 1 :> 0 :. 2
-- Just 6
--
-- @since 0.1.0
(??) :: (Manifest r ix e, MonadThrow m) => m (Array r ix e) -> ix -> m e
(??) marr ix = marr >>= (!? ix)
{-# INLINE (??) #-}

-- | /O(1)/ - Lookup an element in the array. Returns `Nothing`, when index is out of bounds and
-- returns the element at the supplied index otherwise. Use `indexM` instead, since it is more
-- generaland can just as well be used with `Maybe`.
--
-- @since 0.1.0
index :: Manifest r ix e => Array r ix e -> ix -> Maybe e
index = indexM
{-# INLINE index #-}

-- | /O(1)/ - Lookup an element in the array. Throws `IndexOutOfBoundsException`, when index is out
-- of bounds and returns the element at the supplied index otherwise.
--
-- @since 0.3.0
indexM :: (Manifest r ix e, MonadThrow m) => Array r ix e -> ix -> m e
indexM = evaluateM
{-# INLINE indexM #-}

-- | /O(1)/ - Lookup an element in the array, while using default element when index is out of
-- bounds.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XOverloadedLists
-- >>> xs = [0..100] :: Array P Ix1 Int
-- >>> defaultIndex 999 xs 100
-- 100
-- >>> defaultIndex 999 xs 101
-- 999
--
-- @since 0.1.0
defaultIndex :: Manifest r ix e => e -> Array r ix e -> ix -> e
defaultIndex defVal = borderIndex (Fill defVal)
{-# INLINE defaultIndex #-}

-- | /O(1)/ - Lookup an element in the array. Use a border resolution technique
-- when index is out of bounds.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XOverloadedLists
-- >>> xs = [0..100] :: Array U Ix1 Int
-- >>> borderIndex Wrap xs <$> range Seq 99 104
-- Array D Seq (Sz1 5)
--   [ 99, 100, 0, 1, 2 ]
--
-- @since 0.1.0
borderIndex :: Manifest r ix e => Border e -> Array r ix e -> ix -> e
borderIndex border arr = handleBorderIndex border (size arr) (unsafeIndex arr)
{-# INLINE borderIndex #-}

-- | /O(1)/ - Lookup an element in the array. This is a partial function and it can throw
-- `IndexOutOfBoundsException` inside pure code. It is safer to use `index` instead.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XOverloadedLists
-- >>> xs = [0..100] :: Array U Ix1 Int
-- >>> index' xs 50
-- 50
-- >>> index' xs 150
-- *** Exception: IndexOutOfBoundsException: 150 is not safe for (Sz1 101)
--
-- @since 0.1.0
index' :: Manifest r ix e => Array r ix e -> ix -> e
index' = evaluate'
{-# INLINE index' #-}

-- | This is just like `indexM` function, but it allows getting values from
-- delayed arrays as well as `Manifest`. As the name suggests, indexing into a
-- delayed array at the same index multiple times will cause evaluation of the
-- value each time and can destroy the performace if used without care.
--
-- ==== __Examples__
--
-- >>> import Control.Exception
-- >>> import Data.Massiv.Array
-- >>> evaluateM (range Seq (Ix2 10 20) (100 :. 210)) 50 :: Either SomeException Ix2
-- Right (60 :. 70)
-- >>> evaluateM (range Seq (Ix2 10 20) (100 :. 210)) 150 :: Either SomeException Ix2
-- Left (IndexOutOfBoundsException: (150 :. 150) is not safe for (Sz (90 :. 190)))
--
-- @since 0.3.0
evaluateM :: (Source r ix e, MonadThrow m) => Array r ix e -> ix -> m e
evaluateM arr ix =
  handleBorderIndex
    (Fill (throwM (IndexOutOfBoundsException (size arr) ix)))
    (size arr)
    (pure . unsafeIndex arr)
    ix
{-# INLINE evaluateM #-}

-- | Similar to `evaluateM`, but will throw an exception in pure code.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> evaluate' (range Seq (Ix2 10 20) (100 :. 210)) 50
-- 60 :. 70
-- >>> evaluate' (range Seq (Ix2 10 20) (100 :. 210)) 150
-- *** Exception: IndexOutOfBoundsException: (150 :. 150) is not safe for (Sz (90 :. 190))
--
-- @since 0.3.0
evaluate' :: Source r ix e => Array r ix e -> ix -> e
evaluate' arr ix =
  handleBorderIndex
    (Fill (throw (IndexOutOfBoundsException (size arr) ix)))
    (size arr)
    (unsafeIndex arr)
    ix
{-# INLINE evaluate' #-}


-- | Map a monadic index aware function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> imapM_ (curry print) $ range Seq (Ix1 10) 15
-- (0,10)
-- (1,11)
-- (2,12)
-- (3,13)
-- (4,14)
--
-- @since 0.1.0
imapM_ :: (Source r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f !arr =
  iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}

-- | Apply a function to each element of an array.
--
-- @since 0.4.1
liftArray ::
     forall r ix e r' b. (Construct r ix e, Source r' ix b)
  => (b -> e)
  -> Array r' ix b
  -> Array r ix e
liftArray f a = makeArrayLinear (getComp a) (size a) (f . unsafeLinearIndex a)
{-# INLINE liftArray #-}

-- | Apply a function to each element of two arrays pointwise. Similarly to
-- `unsafeLiftArray2`, except that `SizeMismatchException` is thrown when the size of both
-- arrays does not match.
--
-- @since 0.4.1
liftArray2M ::
     forall r ix e r1 r2 e1 e2 m. (Construct r ix e, Source r2 ix e2, Source r1 ix e1, MonadThrow m)
  => (e1 -> e2 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> m (Array r ix e)
liftArray2M f a1 a2
  | size a1 == size a2 = pure $ unsafeLiftArray2 f a1 a2
  | otherwise = throwM $ SizeMismatchException (size a1) (size a2)
{-# INLINE liftArray2M #-}

-- | Same as `liftArray2M`, except it throws the exception purely.
--
-- @since 0.4.1
liftArray2' ::
     forall r ix e r1 r2 e1 e2. (Construct r ix e, Source r2 ix e2, Source r1 ix e1)
  => (e1 -> e2 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r ix e
liftArray2' f a1 a2 = either throw id $ liftArray2M f a1 a2
{-# INLINE liftArray2' #-}

-- | Apply a function to each element of two arrays pointwise. Size of the first array is used.
--
-- @since 0.4.1
unsafeLiftArray2 ::
     (Construct r3 ix e3, Source r2 ix e2, Source r1 ix e1)
  => (e1 -> e2 -> e3)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
unsafeLiftArray2 f a1 a2 =
  makeArrayLinear (getComp a1 <> getComp a2) (size a1) $ \ !i ->
    f (unsafeLinearIndex a1 i) (unsafeLinearIndex a2 i)
{-# INLINE unsafeLiftArray2 #-}


-- | /O(1)/ - Get the number of elements in the array
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> elemsCount $ range Seq (Ix1 10) 15
-- 5
--
-- @since 0.1.0
elemsCount :: Load r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array has no elements.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> isEmpty $ range Seq (Ix2 10 20) (11 :. 21)
-- False
-- >>> isEmpty $ range Seq (Ix2 10 20) (10 :. 21)
-- True
--
-- @since 0.1.0
isEmpty :: Load r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}
