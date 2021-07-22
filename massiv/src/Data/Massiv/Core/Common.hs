{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Core.Common
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.Common
  ( Array
  , Vector
  , MVector
  , Matrix
  , MMatrix
  , Elt
  , Steps(..)
  , Stream(..)
  , Strategy(..)
  , Source(..)
  , Load(..)
  , StrideLoad(..)
  , Size(..)
  , Shape(..)
  , Resize(..)
  , Manifest(..)
  , Mutable(..)
  , Comp(..)
  , Scheduler
  , numWorkers
  , scheduleWork
  , scheduleWork_
  , withMassivScheduler_
  , WorkerStates
  , unsafeRead
  , unsafeWrite
  , unsafeModify
  , unsafeLinearModify
  , unsafeSwap
  , unsafeLinearSwap
  , unsafeDefaultLinearShrink
  , Ragged(..)
  , empty
  , singleton
  -- * Size
  , elemsCount
  , isNotNull
  , isEmpty
  , isNotEmpty
  , Sz(SafeSz)
  , LengthHint(..)
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
  , inline0
  , inline1
  , inline2
  , module Data.Massiv.Core.Index
  -- * Common Operations
  , imapM_
  , Semigroup((<>))
  -- * Exceptions
  , MonadThrow(..)
  , IndexException(..)
  , SizeException(..)
  , ShapeException(..)
  , module Data.Massiv.Core.Exception
  , Proxy(..)
  , Id(..)
  -- * Stateful Monads
  , runST
  , ST
  , MonadUnliftIO(..)
  , MonadIO(liftIO)
  , PrimMonad(PrimState)
  , RealWorld
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Unlift (MonadIO(liftIO), MonadUnliftIO(..))
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Scheduler (Comp(..), Scheduler, WorkerStates, numWorkers,
                          scheduleWork, scheduleWork_, trivialScheduler_,
                          withScheduler_)
import Control.Scheduler.Global
import GHC.Exts (IsList)
import Data.Massiv.Core.Exception
import Data.Massiv.Core.Index
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Data.Typeable
import Data.Kind
import qualified Data.Vector.Fusion.Stream.Monadic as S (Stream)
import Data.Vector.Fusion.Util

#include "massiv.h"

-- | The array family. Representations @r@ describe how data is arranged or computed. All
-- arrays have a common property that each index @ix@ always maps to the same unique
-- element @e@, even if that element does not yet exist in memory and the array has to be
-- computed in order to get access to that element. Data is always arranged in a nested
-- row-major fashion. Rank of an array is specified by @`Dimensions` ix@.
data family Array r ix e :: Type

-- | Type synonym for a single dimension array, or simply a flat vector.
--
-- @since 0.5.0
type Vector r e = Array r Ix1 e


-- | Type synonym for a single dimension mutable array, or simply a flat mutable vector.
--
-- @since 0.5.0
type MVector s r e = MArray s r Ix1 e

-- | Type synonym for a two-dimentsional array, or simply a matrix.
--
-- @since 0.5.0
type Matrix r e = Array r Ix2 e


-- | Type synonym for a two-dimentsional mutable array, or simply a mutable matrix.
--
-- @since 0.5.0
type MMatrix s r e = MArray s r Ix2 e



type family Elt r ix e :: Type where
  Elt r Ix1 e = e
  Elt r ix  e = Array r (Lower ix) e


class Load r ix e => Stream r ix e where
  toStream :: Array r ix e -> Steps Id e

  toStreamIx :: Array r ix e -> Steps Id (ix, e)

data Steps m e = Steps
  { stepsStream :: S.Stream m e
  , stepsSize   :: LengthHint
  }

class Typeable r => Strategy r where
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

  -- | Get computation strategy of this array
  --
  -- @since 0.1.0
  getComp :: Array r ix e -> Comp


-- | Size hint
--
-- @since 1.0.0
data LengthHint
  = LengthExact Sz1 -- ^ Exact known size
  | LengthMax Sz1 -- ^ Upper bound on the size
  | LengthUnknown -- ^ Unknown size
  deriving (Eq, Show)


-- | The shape of an array. It is different from `Size` in that it can be applicable to
-- non-square matrices and might not be available in constant time.
--
-- @since 1.0.0
class Index ix => Shape r ix where

  -- | /O(1)/ - Check what do we know about the number of elements without doing any work
  --
  -- @since 1.0.0
  linearSizeHint :: Array r ix e -> LengthHint
  linearSizeHint = LengthExact . linearSize
  {-# INLINE linearSizeHint #-}

  -- | /O(n)/ - possibly iterate over the whole array before producing the answer
  --
  -- @since 0.5.8
  linearSize :: Array r ix e -> Sz1
  default linearSize :: Size r => Array r ix e -> Sz1
  linearSize = SafeSz . elemsCount
  {-# INLINE linearSize #-}

  -- | /O(n)/ - Rectangular size of an array that is inferred from looking at the first row in
  -- each dimensions. For rectangular arrays this is the same as `size`
  --
  -- @since 1.0.0
  outerSize :: Array r ix e -> Sz ix
  default outerSize :: Size r => Array r ix e -> Sz ix
  outerSize = size
  {-# INLINE outerSize #-}

  -- | /O(1)/ - Get the possible maximum linear size of an immutabe array. If the lookup
  -- of size in constant time is not possible, `Nothing` will be returned. This value
  -- will be used as the initial size of the mutable array into which the loading will
  -- happen.
  --
  -- @since 1.0.0
  maxLinearSize :: Array r ix e -> Maybe Sz1
  maxLinearSize = lengthHintUpperBound . linearSizeHint
  {-# INLINE maxLinearSize #-}

  -- | /O(1)/ - Check whether an array is empty or not.
  --
  -- ==== __Examples__
  --
  -- >>> import Data.Massiv.Array
  -- >>> isNull $ range Seq (Ix2 10 20) (11 :. 21)
  -- False
  -- >>> isNull $ range Seq (Ix2 10 20) (10 :. 21)
  -- True
  -- >>> isNull (empty :: Array D Ix5 Int)
  -- True
  -- >>> isNull $ sfromList []
  -- True
  --
  -- @since 1.0.0
  isNull :: Array r ix e -> Bool
  isNull = (zeroSz ==) . linearSize
  {-# INLINE isNull #-}


lengthHintUpperBound :: LengthHint -> Maybe Sz1
lengthHintUpperBound = \case
    LengthExact sz -> Just sz
    LengthMax sz   -> Just sz
    LengthUnknown  -> Nothing
{-# INLINE lengthHintUpperBound #-}


class Size r where

  -- | Get the exact size of an immutabe array. Most of the time will produce
  -- the size in constant time, except for `Data.Massiv.Array.DS`
  -- representation, which could result in evaluation of the whole stream. See
  -- `maxLinearSize` and `Data.Massiv.Vector.slength` for more info.
  --
  -- @since 0.1.0
  size :: Array r ix e -> Sz ix

class Size r => Resize r where
  -- | /O(1)/ - Change the size of an array. Total number of elements should be the same, but it is
  -- not validated.
  unsafeResize :: (Index ix, Index ix') => Sz ix' -> Array r ix e -> Array r ix' e



-- | Arrays that can be used as source to practically any manipulation function.
class (Strategy r, Resize r) => Source r e where
  {-# MINIMAL (unsafeIndex|unsafeLinearIndex), unsafeLinearSlice #-}

  -- | Lookup element in the array. No bounds check is performed and access of
  -- arbitrary memory is possible when invalid index is supplied.
  --
  -- @since 0.1.0
  unsafeIndex :: Index ix => Array r ix e -> ix -> e
  unsafeIndex =
    INDEX_CHECK("(Source r e).unsafeIndex",
                size, \ !arr -> unsafeLinearIndex arr . toLinearIndex (size arr))
  {-# INLINE unsafeIndex #-}

  -- | Lookup element in the array using flat index in a row-major fashion. No
  -- bounds check is performed
  --
  -- @since 0.1.0
  unsafeLinearIndex :: Index ix => Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}


  -- | /O(1)/ - Take a slice out of an array from the outside
  unsafeOuterSlice :: (Index ix, Index (Lower ix)) =>
    Array r ix e -> Sz (Lower ix) -> Int -> Array r (Lower ix) e
  unsafeOuterSlice arr sz i = unsafeResize sz $ unsafeLinearSlice i (toLinearSz sz) arr
  {-# INLINE unsafeOuterSlice #-}

  -- | /O(1)/ - Source arrays also give us ability to look at their linear slices in
  -- constant time
  --
  -- @since 0.5.0
  unsafeLinearSlice :: Index ix => Ix1 -> Sz1 -> Array r ix e -> Array r Ix1 e


-- | Any array that can be computed and loaded into memory
class (Strategy r, Shape r ix) => Load r ix e where
  {-# MINIMAL (makeArray | makeArrayLinear), (loadArrayM | loadArrayWithSetM)#-}

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
  -- Instead of restricting the full type manually we can use @TypeApplications@ as convenience:
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


  -- | Construct an array of the specified size that contains the same element in all of
  -- the cells.
  --
  -- @since 0.3.0
  replicate :: Comp -> Sz ix -> e -> Array r ix e
  replicate comp sz !e = makeArrayLinear comp sz (const e)
  {-# INLINE replicate #-}


  -- | Load an array into memory.
  --
  -- @since 0.3.0
  loadArrayM
    :: Scheduler s ()
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> ST s ()) -- ^ Function that writes an element into target array
    -> ST s ()
  loadArrayM scheduler arr uWrite =
    loadArrayWithSetM scheduler arr uWrite $ \offset sz e ->
      loopM_ offset (< (offset + unSz sz)) (+1) (`uWrite` e)
  {-# INLINE loadArrayM #-}

  -- | Load an array into memory, just like `loadArrayM`. Except it also accepts a
  -- function that is potentially optimized for setting many cells in a region to the same
  -- value
  --
  -- @since 0.5.8
  loadArrayWithSetM
    :: Scheduler s ()
    -> Array r ix e -- ^ Array that is being loaded
    -> (Ix1 -> e -> ST s ()) -- ^ Function that writes an element into target array
    -> (Ix1 -> Sz1 -> e -> ST s ()) -- ^ Function that efficiently sets a region of an array
                                    -- to the supplied value target array
    -> ST s ()
  loadArrayWithSetM scheduler arr uWrite _ = loadArrayM scheduler arr uWrite
  {-# INLINE loadArrayWithSetM #-}


  -- | Load into a supplied mutable array sequentially. Returned array does not have to be
  -- the same
  --
  -- @since 0.5.7
  unsafeLoadIntoS ::
       Mutable r' e
    => MVector s r' e
    -> Array r ix e
    -> ST s (MArray s r' ix e)
  unsafeLoadIntoS marr arr =
    munsafeResize (outerSize arr) marr <$
    loadArrayWithSetM trivialScheduler_ arr (unsafeLinearWrite marr) (unsafeLinearSet marr)
  {-# INLINE unsafeLoadIntoS #-}

  -- | Same as `unsafeLoadIntoS`, but respecting computation strategy.
  --
  -- @since 0.5.7
  unsafeLoadIntoM ::
       Mutable r' e
    => MVector RealWorld r' e
    -> Array r ix e
    -> IO (MArray RealWorld r' ix e)
  unsafeLoadIntoM marr arr = do
    withMassivScheduler_ (getComp arr) $ \scheduler ->
      stToIO $ loadArrayWithSetM scheduler arr (unsafeLinearWrite marr) (unsafeLinearSet marr)
    pure $ munsafeResize (outerSize arr) marr
  {-# INLINE unsafeLoadIntoM #-}

-- | Selects an optimal scheduler for the supplied strategy, but it works only in `IO`
--
-- @since 1.0.0
withMassivScheduler_ :: Comp -> (Scheduler RealWorld () -> IO ()) -> IO ()
withMassivScheduler_ comp f =
  case comp of
    Par -> withGlobalScheduler_ globalScheduler f
    Seq -> f trivialScheduler_
    _   -> withScheduler_ comp f
{-# INLINE withMassivScheduler_ #-}

class (Size r, Load r ix e) => StrideLoad r ix e where
  -- | Load an array into memory with stride. Default implementation requires an instance of
  -- `Source`.
  loadArrayWithStrideM
    :: Scheduler s ()
    -> Stride ix -- ^ Stride to use
    -> Sz ix -- ^ Size of the target array affected by the stride.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> ST s ()) -- ^ Function that writes an element into target array
    -> ST s ()
  default loadArrayWithStrideM
    :: Source r e =>
       Scheduler s ()
    -> Stride ix
    -> Sz ix
    -> Array r ix e
    -> (Int -> e -> ST s ())
    -> ST s ()
  loadArrayWithStrideM scheduler stride resultSize arr =
    splitLinearlyWith_ scheduler (totalElem resultSize) unsafeLinearWriteWithStride
    where
      !strideIx = unStride stride
      unsafeLinearWriteWithStride =
        unsafeIndex arr . liftIndex2 (*) strideIx . fromLinearIndex resultSize
      {-# INLINE unsafeLinearWriteWithStride #-}
  {-# INLINE loadArrayWithStrideM #-}

-- class (Load r ix e) => StrideLoad r ix e where
-- class (Size r, StrideLoad r ix e) => StrideLoadP r ix e where
  --
  -- unsafeLoadIntoWithStrideST ::
  --      Mutable r' ix e
  --   => Array r ix e
  --   -> Stride ix -- ^ Stride to use
  --   -> MArray RealWorld r' ix e
  --   -> m (MArray RealWorld r' ix e)


--TODO: rethink Size here to support outer slicing (Something like OuterSize?) Affects
--only ragged arrays (L, LN and DS don't count, since they don't have constant time
--slicing anyways)

-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- @(`!`)@, @(`!?`)@, etc. are constrained to manifest arrays only.
class (Resize r, Source r e) => Manifest r e where

  unsafeLinearIndexM :: Index ix => Array r ix e -> Int -> e


class Manifest r e => Mutable r e where
  data MArray s r ix e :: Type

  -- | Get the size of a mutable array.
  --
  -- @since 0.1.0
  msize :: Index ix => MArray s r ix e -> Sz ix

  -- | Get the size of a mutable array.
  --
  -- @since 0.1.0
  munsafeResize :: (Index ix', Index ix) => Sz ix' -> MArray s r ix e -> MArray s r ix' e

  -- | Convert immutable array into a mutable array without copy.
  --
  -- @since 0.1.0
  unsafeThaw :: (Index ix, PrimMonad m) => Array r ix e -> m (MArray (PrimState m) r ix e)

  -- | Convert mutable array into an immutable array without copy.
  --
  -- @since 0.1.0
  unsafeFreeze :: (Index ix, PrimMonad m) => Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)

  -- | Create new mutable array, leaving it's elements uninitialized. Size isn't validated either.
  --
  -- @since 0.1.0
  unsafeNew :: (Index ix, PrimMonad m) => Sz ix -> m (MArray (PrimState m) r ix e)

  -- | Read an element at linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearRead :: (Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> Int -> m e

  -- | Write an element into mutable array with linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearWrite :: (Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> Int -> e -> m ()

  -- | Initialize mutable array to some default value.
  --
  -- @since 0.3.0
  initialize :: (Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> m ()

  -- | Create new mutable array while initializing all elements to some default value.
  --
  -- @since 0.3.0
  initializeNew :: (Index ix, PrimMonad m) => Maybe e -> Sz ix -> m (MArray (PrimState m) r ix e)
  initializeNew Nothing sz = unsafeNew sz >>= \ma -> ma <$ initialize ma
  initializeNew (Just e) sz = newMArray sz e
  {-# INLINE initializeNew #-}

  -- | Create new mutable array while initializing all elements to the specified value.
  --
  -- @since 0.6.0
  newMArray :: (Index ix, PrimMonad m) => Sz ix -> e -> m (MArray (PrimState m) r ix e)
  newMArray sz e = do
    marr <- unsafeNew sz
    marr <$ unsafeLinearSet marr 0 (SafeSz (totalElem sz)) e
  {-# INLINE newMArray #-}

  -- | Set all cells in the mutable array within the range to a specified value.
  --
  -- @since 0.3.0
  unsafeLinearSet :: (Index ix, PrimMonad m) =>
                     MArray (PrimState m) r ix e -> Ix1 -> Sz1 -> e -> m ()
  unsafeLinearSet marr offset len e =
    loopM_ offset (< (offset + unSz len)) (+1) (\i -> unsafeLinearWrite marr i e)
  {-# INLINE unsafeLinearSet #-}

  -- | Copy part of one mutable array into another
  --
  -- @since 0.3.6
  unsafeLinearCopy :: (Index ix', Index ix, PrimMonad m) =>
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
  unsafeArrayLinearCopy :: (Index ix', Index ix, PrimMonad m) =>
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
  unsafeLinearShrink :: (Index ix, PrimMonad m) =>
                        MArray (PrimState m) r ix e -> Sz ix -> m (MArray (PrimState m) r ix e)
  unsafeLinearShrink = unsafeDefaultLinearShrink
  {-# INLINE unsafeLinearShrink #-}

  -- | Linearly increase the size of an array. Total number of elements should be larger
  -- or equal. There is no guarantee that the original array is left unchanged, so it
  -- should no longer be used.
  --
  -- @since 0.3.6
  unsafeLinearGrow :: (Index ix, PrimMonad m) =>
                      MArray (PrimState m) r ix e -> Sz ix -> m (MArray (PrimState m) r ix e)
  unsafeLinearGrow marr sz = do
    marr' <- unsafeNew sz
    unsafeLinearCopy marr 0 marr' 0 $ SafeSz (totalElem (msize marr))
    pure marr'
  {-# INLINE unsafeLinearGrow #-}


unsafeDefaultLinearShrink ::
     (Mutable r e, Index ix, PrimMonad m)
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
unsafeRead :: (Mutable r e, Index ix, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> m e
unsafeRead marr = unsafeLinearRead marr . toLinearIndex (msize marr)
{-# INLINE unsafeRead #-}

-- | Write an element into array
--
-- @since 0.1.0
unsafeWrite :: (Mutable r e, Index ix, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> e -> m ()
unsafeWrite marr = unsafeLinearWrite marr . toLinearIndex (msize marr)
{-# INLINE unsafeWrite #-}


-- | Modify an element in the array with a monadic action. Returns the previous value.
--
-- @since 0.4.0
unsafeLinearModify :: (Mutable r e, Index ix, PrimMonad m) =>
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
unsafeModify :: (Mutable r e, Index ix, PrimMonad m) =>
                MArray (PrimState m) r ix e -> (e -> m e) -> ix -> m e
unsafeModify marr f ix = unsafeLinearModify marr f (toLinearIndex (msize marr) ix)
{-# INLINE unsafeModify #-}

-- | Swap two elements in a mutable array under the supplied indices. Returns the previous
-- values.
--
-- @since 0.4.0
unsafeSwap :: (Mutable r e, Index ix, PrimMonad m) =>
              MArray (PrimState m) r ix e -> ix -> ix -> m (e, e)
unsafeSwap !marr !ix1 !ix2 = unsafeLinearSwap marr (toLinearIndex sz ix1) (toLinearIndex sz ix2)
  where sz = msize marr
{-# INLINE unsafeSwap #-}


-- | Swap two elements in a mutable array under the supplied linear indices. Returns the
-- previous values.
--
-- @since 0.4.0
unsafeLinearSwap :: (Mutable r e, Index ix, PrimMonad m) =>
                    MArray (PrimState m) r ix e -> Int -> Int -> m (e, e)
unsafeLinearSwap !marr !i1 !i2 = do
  val1 <- unsafeLinearRead marr i1
  val2 <- unsafeLinearRead marr i2
  unsafeLinearWrite marr i1 val2
  unsafeLinearWrite marr i2 val1
  return (val1, val2)
{-# INLINE unsafeLinearSwap #-}


class (IsList (Array r ix e), Load r ix e) => Ragged r ix e where

  emptyR :: Comp -> Array r ix e

  consR :: Elt r ix e -> Array r ix e -> Array r ix e

  unconsR :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  generateRaggedM :: Monad m => Comp -> Sz ix -> (ix -> m e) -> m (Array r ix e)

  flattenRagged :: Array r ix e -> Vector r e

  loadRagged ::
    Scheduler s () -> (Ix1 -> e -> ST s a) -> Ix1 -> Ix1 -> Sz ix -> Array r ix e -> ST s ()

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
     forall r ix e. Load r ix e
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
     forall r ix e. Load r ix e
  => e -- ^ The only element
  -> Array r ix e
singleton = makeArray Seq oneSz . const
{-# INLINE singleton #-}


infixl 4 !, !?, ??

-- | /O(1)/ - Infix version of `index'`.
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
--
-- @since 0.1.0
(!) ::
     forall r ix e. (HasCallStack, Manifest r e, Index ix)
  => Array r ix e
  -> ix
  -> e
(!) arr = throwEither . evaluateM arr
{-# INLINE (!) #-}


-- | /O(1)/ - Infix version of `indexM`.
--
-- /__Exceptions__/: `IndexOutOfBoundsException`
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
(!?) ::
     forall r ix e m. (Index ix, Manifest r e, MonadThrow m)
  => Array r ix e
  -> ix
  -> m e
(!?) = indexM
{-# INLINE (!?) #-}


-- | /O(1)/ - Lookup an element in the array, where array itself is wrapped with
-- `MonadThrow`. This operator is useful when used together with slicing or other
-- functions that can fail.
--
-- /__Exceptions__/: `IndexOutOfBoundsException`
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
-- Just (Array U Seq (Sz (1 :. 3))
--   [ [ 4, 5, 6 ]
--   ]
-- )
-- >>> ma ??> 1 ?? 0 :. 2
-- Just 6
-- >>> ma ?? 1 :> 0 :. 2
-- Just 6
--
-- @since 0.1.0
(??) :: (Index ix, Manifest r e, MonadThrow m) => m (Array r ix e) -> ix -> m e
(??) marr ix = marr >>= (!? ix)
{-# INLINE (??) #-}

-- | /O(1)/ - Lookup an element in the array. Returns `Nothing`, when index is out of bounds and
-- returns the element at the supplied index otherwise. Use `indexM` instead, since it is more
-- general and it can just as well be used with `Maybe`.
--
-- @since 0.1.0
index :: (Index ix, Manifest r e) => Array r ix e -> ix -> Maybe e
index = indexM
{-# INLINE index #-}

-- | /O(1)/ - Lookup an element in the array.
--
-- /__Exceptions__/: `IndexOutOfBoundsException`
--
-- @since 0.3.0
indexM :: (Index ix, Manifest r e, MonadThrow m) => Array r ix e -> ix -> m e
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
defaultIndex :: (Index ix, Manifest r e) => e -> Array r ix e -> ix -> e
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
borderIndex :: (Index ix, Manifest r e) => Border e -> Array r ix e -> ix -> e
borderIndex border arr = handleBorderIndex border (size arr) (unsafeIndex arr)
{-# INLINE borderIndex #-}

-- | /O(1)/ - Lookup an element in the array. This is a partial function and it will throw
-- an error when index is out of bounds. It is safer to use `indexM` instead.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XOverloadedLists
-- >>> xs = [0..100] :: Array U Ix1 Int
-- >>> index' xs 50
-- 50
--
-- @since 0.1.0
index' :: (HasCallStack, Index ix, Manifest r e) => Array r ix e -> ix -> e
index' arr = throwEither . evaluateM arr
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
evaluateM :: (Index ix, Source r e, MonadThrow m) => Array r ix e -> ix -> m e
evaluateM arr ix =
  handleBorderIndex
    (Fill (throwM (IndexOutOfBoundsException (size arr) ix)))
    (size arr)
    (pure . unsafeIndex arr)
    ix
{-# INLINE evaluateM #-}

-- | Similar to `evaluateM`, but will throw an error on out of bounds indices.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> evaluate' (range Seq (Ix2 10 20) (100 :. 210)) 50
-- 60 :. 70
--
-- @since 0.3.0
evaluate' :: (HasCallStack, Index ix, Source r e) => Array r ix e -> ix -> e
evaluate' arr = throwEither . evaluateM arr
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
imapM_ :: (Index ix, Source r a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f !arr =
  iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}



-- | /O(1)/ - Check if array has elements.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> isNotNull (singleton 1 :: Array D Ix2 Int)
-- True
-- >>> isNotNull (empty :: Array D Ix2 Int)
-- False
--
-- @since 0.5.1
isNotNull :: Shape r ix => Array r ix e -> Bool
isNotNull = not . isNull
{-# INLINE isNotNull #-}



-- | /O(1)/ - Check if array has elements.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> isEmpty (singleton 1 :: Array D Ix2 Int)
-- False
-- >>> isEmpty (empty :: Array D Ix2 Int)
-- True
--
-- @since 1.0.0
isEmpty :: (Index ix, Size r) => Array r ix e -> Bool
isEmpty = (==0) . elemsCount
{-# INLINE isEmpty #-}


-- | /O(1)/ - Check if array has elements.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> isNotEmpty (singleton 1 :: Array D Ix2 Int)
-- True
-- >>> isNotEmpty (empty :: Array D Ix2 Int)
-- False
--
-- @since 1.0.0
isNotEmpty :: (Index ix, Size r) => Array r ix e -> Bool
isNotEmpty = not . isEmpty
{-# INLINE isNotEmpty #-}


-- | /O(1)/ - Get the number of elements in the array.
--
-- /Note/ - It is always a constant time operation except for some arrays with
-- `Data.Massiv.Array.DS` representation. See `Data.Massiv.Vector.slength` for more info.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> elemsCount $ range Seq (Ix1 10) 15
-- 5
--
-- @since 0.1.0
elemsCount :: (Index ix, Size r) => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}


inline0 :: (a -> b) -> a -> b
inline0 f = f
{-# INLINE [0] inline0 #-}

inline1 :: (a -> b) -> a -> b
inline1 f = f
{-# INLINE [1] inline1 #-}

inline2 :: (a -> b) -> a -> b
inline2 f = f
{-# INLINE [2] inline2 #-}
