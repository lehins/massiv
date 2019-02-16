{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Internal
  ( M
  , Manifest(..)
  , Array(..)
  , makeBoxedVector
  , toManifest
  , compute
  , computeS
  , computeAs
  , computeProxy
  , computeSource
  , computeWithStride
  , computeWithStrideAs
  , clone
  , convert
  , convertAs
  , convertProxy
  , gcastArr
  , fromRaggedArrayM
  , fromRaggedArray'
  , fromRaggedArray
  , sizeofArray
  , sizeofMutableArray
  ) where

import           Control.Exception                   (try)
import           Control.Monad.ST
import qualified Data.Foldable                       as F (Foldable (..))
import           Data.Massiv.Array.Delayed.Pull
import           Data.Massiv.Array.Ops.Fold.Internal
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           Data.Massiv.Scheduler
import           Data.Maybe                          (fromMaybe)
import           Data.Typeable
import qualified Data.Vector                         as V
import qualified Data.Vector.Mutable                 as MV
import           GHC.Base                            hiding (ord)
import           System.IO.Unsafe                    (unsafePerformIO)

#if MIN_VERSION_primitive(0,6,2)
import           Data.Primitive.Array                (sizeofArray,
                                                      sizeofMutableArray)

#else
import qualified Data.Primitive.Array                as A (Array (..),
                                                           MutableArray (..))
import           GHC.Prim                            (sizeofArray#,
                                                      sizeofMutableArray#)

sizeofArray :: A.Array a -> Int
sizeofArray (A.Array a) = I# (sizeofArray# a)
{-# INLINE sizeofArray #-}

sizeofMutableArray :: A.MutableArray s a -> Int
sizeofMutableArray (A.MutableArray ma) = I# (sizeofMutableArray# ma)
{-# INLINE sizeofMutableArray #-}
#endif


-- | General Manifest representation
data M

data instance Array M ix e = MArray { mComp :: !Comp
                                    , mSize :: !(Sz ix)
                                    , mLinearIndex :: Int -> e }
type instance EltRepr M ix = M

instance (Ragged L ix e, Show e) => Show (Array M ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList


instance (Eq e, Index ix) => Eq (Array M ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (Ord e, Index ix) => Ord (Array M ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance Index ix => Construct M ix e where
  setComp c arr = arr {mComp = c}
  {-# INLINE setComp #-}
  makeArrayLinear !comp !sz f =
    unsafePerformIO $ do
      let !k = totalElem sz
      mv <- MV.unsafeNew k
      withScheduler_ comp $ \s ->
        splitLinearlyWithM_ (numWorkers s) (scheduleWork s) k (pure . f) (MV.unsafeWrite mv)
      v <- V.unsafeFreeze mv
      pure $ MArray comp sz (V.unsafeIndex v)
  {-# INLINE makeArrayLinear #-}

-- | Create a boxed from usual size and index to element function
makeBoxedVector :: Index ix => Sz ix -> (ix -> a) -> V.Vector a
makeBoxedVector !sz f = V.generate (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE makeBoxedVector #-}


-- | /O(1)/ - Conversion of `Manifest` arrays to `M` representation.
toManifest :: Manifest r ix e => Array r ix e -> Array M ix e
toManifest !arr = MArray (getComp arr) (size arr) (unsafeLinearIndexM arr)
{-# INLINE toManifest #-}


-- | Row-major sequentia folding over a Manifest array.
instance Index ix => Foldable (Array M ix) where
  fold = fold
  {-# INLINE fold #-}
  foldMap = foldMono
  {-# INLINE foldMap #-}
  foldl = lazyFoldlS
  {-# INLINE foldl #-}
  foldl' = foldlS
  {-# INLINE foldl' #-}
  foldr = foldrFB
  {-# INLINE foldr #-}
  foldr' = foldrS
  {-# INLINE foldr' #-}
  null (MArray _ sz _) = totalElem sz == 0
  {-# INLINE null #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}



instance Index ix => Source M ix e where
  unsafeLinearIndex = mLinearIndex
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Manifest M ix e where

  unsafeLinearIndexM = mLinearIndex
  {-# INLINE unsafeLinearIndexM #-}


instance Index ix => Resize Array M ix where
  unsafeResize !sz !arr = arr { mSize = sz }
  {-# INLINE unsafeResize #-}

instance Index ix => Extract M ix e where
  unsafeExtract !sIx !newSz !arr =
    MArray (getComp arr) newSz $ \ i ->
      unsafeIndex arr (liftIndex2 (+) (fromLinearIndex newSz i) sIx)
  {-# INLINE unsafeExtract #-}



instance {-# OVERLAPPING #-} Slice M Ix1 e where
  unsafeSlice arr i _ _ = pure (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         ) =>
         Slice M ix e where
  unsafeSlice arr start cutSz dim = do
    (_, newSz) <- pullOutSzM cutSz dim
    return $ unsafeResize newSz (unsafeExtract start cutSz arr)
  {-# INLINE unsafeSlice #-}

instance {-# OVERLAPPING #-} OuterSlice M Ix1 e where
  unsafeOuterSlice !arr = unsafeIndex arr
  {-# INLINE unsafeOuterSlice #-}

instance (Elt M ix e ~ Array M (Lower ix) e, Index ix, Index (Lower ix)) => OuterSlice M ix e where
  unsafeOuterSlice !arr !i =
    MArray (getComp arr) (snd (unconsSz (size arr))) (unsafeLinearIndex arr . (+ kStart))
    where
      !kStart = toLinearIndex (size arr) (consDim i (zeroIndex :: Lower ix))
  {-# INLINE unsafeOuterSlice #-}

instance {-# OVERLAPPING #-} InnerSlice M Ix1 e where
  unsafeInnerSlice !arr _ = unsafeIndex arr
  {-# INLINE unsafeInnerSlice #-}

instance (Elt M ix e ~ Array M (Lower ix) e, Index ix, Index (Lower ix)) => InnerSlice M ix e where
  unsafeInnerSlice !arr (szL, m) !i =
    MArray (getComp arr) szL (\k -> unsafeLinearIndex arr (k * unSz m + kStart))
    where
      !kStart = toLinearIndex (size arr) (snocDim (zeroIndex :: Lower ix) i)
  {-# INLINE unsafeInnerSlice #-}


instance Index ix => Load M ix e where
  size = mSize
  {-# INLINE size #-}
  getComp = mComp
  {-# INLINE getComp #-}
  loadArrayM numWorkers' scheduleWork' (MArray _ sz f) =
    splitLinearlyWith_ numWorkers' scheduleWork' (totalElem sz) f
  {-# INLINE loadArrayM #-}

instance Index ix => StrideLoad M ix e


-- | Ensure that Array is computed, i.e. represented with concrete elements in memory, hence is the
-- `Mutable` type class restriction. Use `setComp` if you'd like to change computation strategy
-- before calling @compute@
compute :: (Load r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
compute !arr = unsafePerformIO $ loadArray arr >>= unsafeFreeze (getComp arr)
{-# INLINE compute #-}

computeS :: (Load r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
computeS !arr = runST $ loadArrayS arr >>= unsafeFreeze (getComp arr)
{-# INLINE computeS #-}

-- | Just as `compute`, but let's you supply resulting representation type as an argument.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> computeAs P $ range Seq (Ix1 0) 10
-- Array P Seq (Sz1 10)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
--
computeAs :: (Load r' ix e, Mutable r ix e) => r -> Array r' ix e -> Array r ix e
computeAs _ = compute
{-# INLINE computeAs #-}


-- | Same as `compute` and `computeAs`, but let's you supply resulting representation type as a proxy
-- argument.
--
-- ==== __Examples__
--
-- Useful only really for cases when representation constructor or @TypeApplications@ extension
-- aren't desireable for some reason:
--
-- >>> import Data.Proxy
-- >>> import Data.Massiv.Array
-- >>> computeProxy (Proxy :: Proxy P) $ (^ (2 :: Int)) <$> range Seq (Ix1 0) 10
-- Array P Seq (Sz1 10)
--   [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ]
--
-- @since 0.1.1
computeProxy :: (Load r' ix e, Mutable r ix e) => proxy r -> Array r' ix e -> Array r ix e
computeProxy _ = compute
{-# INLINE computeProxy #-}


-- | This is just like `compute`, but can be applied to `Source` arrays and will be a noop if
-- resulting type is the same as the input.
--
-- @since 0.1.0
computeSource :: forall r' r ix e . (Load r' ix e, Source r' ix e, Mutable r ix e)
              => Array r' ix e -> Array r ix e
computeSource arr =
  maybe (compute $ delay arr) (\Refl -> arr) (eqT :: Maybe (r' :~: r))
{-# INLINE computeSource #-}


-- | /O(n)/ - Make an exact immutable copy of an Array.
--
-- @since 0.1.0
clone :: Mutable r ix e => Array r ix e -> Array r ix e
clone arr = unsafePerformIO $ thaw arr >>= unsafeFreeze (getComp arr)
{-# INLINE clone #-}


-- | /O(1)/ - Cast over Array representation
gcastArr :: forall r' r ix e. (Typeable r, Typeable r')
       => Array r' ix e -> Maybe (Array r ix e)
gcastArr arr = fmap (\Refl -> arr) (eqT :: Maybe (r :~: r'))


-- | /O(n)/ - conversion between manifest types, except when source and result arrays
-- are of the same representation, in which case it is an /O(1)/ operation.
--
-- @since 0.1.0
convert :: (Manifest r' ix e, Mutable r ix e)
        => Array r' ix e -> Array r ix e
convert arr =
  fromMaybe (compute $ toManifest arr) (gcastArr arr)
{-# INLINE convert #-}

-- | Same as `convert`, but let's you supply resulting representation type as an argument.
--
-- @since 0.1.0
convertAs :: (Manifest r' ix e, Mutable r ix e)
          => r -> Array r' ix e -> Array r ix e
convertAs _ = convert
{-# INLINE convertAs #-}


-- | Same as `convert` and `convertAs`, but let's you supply resulting representation type as a
-- proxy argument.
--
-- @since 0.1.1
convertProxy :: (Manifest r' ix e, Mutable r ix e)
             => proxy r -> Array r' ix e -> Array r ix e
convertProxy _ = convert
{-# INLINE convertProxy #-}


-- | Convert a ragged array into a usual rectangular shaped one.
fromRaggedArray :: (Ragged r' ix e, Load r' ix e, Mutable r ix e) =>
                   Array r' ix e -> Either ShapeException (Array r ix e)
fromRaggedArray arr =
  unsafePerformIO $ do
    let !sz = edgeSize arr
        !comp = getComp arr
    mArr <- unsafeNew sz
    try $ do
      withScheduler_ comp $ \scheduler ->
        loadRagged (scheduleWork scheduler) (unsafeLinearWrite mArr) 0 (totalElem sz) sz arr
      unsafeFreeze comp mArr
{-# INLINE fromRaggedArray #-}
{-# DEPRECATED fromRaggedArray "In favor of a more general `fromRaggedArrayM`" #-}

-- | Convert a ragged array into a common array with rectangular shaped. Throws `ShapeException`
-- whenever supplied ragged array does not have a rectangular shape.
--
-- @since 0.3.0
fromRaggedArrayM ::
     (Ragged r' ix e, Load r' ix e, Mutable r ix e, MonadThrow m)
  => Array r' ix e
  -> m (Array r ix e)
fromRaggedArrayM arr =
  let sz = edgeSize arr
   in either (\(e :: ShapeException) -> throwM e) pure $
      unsafePerformIO $
      try $
      createArray_ (getComp arr) sz $ \_numWorkers scheduleWork' marr -> do
        loadRagged scheduleWork' (unsafeLinearWrite marr) 0 (totalElem sz) sz arr
{-# INLINE fromRaggedArrayM #-}


-- | Same as `fromRaggedArray`, but will throw an error if its shape is not
-- rectangular.
fromRaggedArray' :: (Load r' ix e, Ragged r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
fromRaggedArray' arr = either throw id $ fromRaggedArrayM arr
{-# INLINE fromRaggedArray' #-}


-- | Same as `compute`, but with `Stride`.
--
-- /O(n div k)/ - Where @n@ is numer of elements in the source array and @k@ is number of elemts in
-- the stride.
--
-- @since 0.3.0
computeWithStride ::
     (StrideLoad r' ix e, Mutable r ix e)
  => Stride ix
  -> Array r' ix e
  -> Array r ix e
computeWithStride stride !arr =
  unsafePerformIO $ do
    let !sz = strideSize stride (size arr)
    createArray_ (getComp arr) sz $ \numWorkers' scheduleWork' marr ->
      loadArrayWithStrideM numWorkers' scheduleWork' stride sz arr (unsafeLinearWrite marr)
{-# INLINE computeWithStride #-}


-- | Same as `computeWithStride`, but with ability to specify resulting array representation.
--
-- @since 0.3.0
computeWithStrideAs ::
     (StrideLoad r' ix e, Mutable r ix e) => r -> Stride ix -> Array r' ix e -> Array r ix e
computeWithStrideAs _ = computeWithStride
{-# INLINE computeWithStrideAs #-}
