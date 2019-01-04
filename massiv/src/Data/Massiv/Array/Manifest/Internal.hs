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
-- Copyright   : (c) Alexey Kuleshevich 2018
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
  , computeAs
  , computeProxy
  , computeSource
  , computeInto
  , computeWithStride
  , computeWithStrideAs
  , clone
  , convert
  , convertAs
  , convertProxy
  , gcastArr
  , loadMutableS
  , loadMutableOnP
  , sequenceP
  , sequenceOnP
  , fromRaggedArray
  , fromRaggedArray'
  , sizeofArray
  , sizeofMutableArray
  ) where

import           Control.Exception                   (try)
import           Control.Monad                       (unless)
import           Control.Monad.ST                    (runST)
import           Data.Foldable                       (Foldable (..))
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Ops.Fold.Internal as M
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           Data.Massiv.Core.Scheduler
import           Data.Maybe                          (fromMaybe)
import           Data.Typeable
import qualified Data.Vector                         as V
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
                                    , mSize :: !ix
                                    , mLinearIndex :: Int -> e }
type instance EltRepr M ix = M

instance (Eq e, Index ix) => Eq (Array M ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (Ord e, Index ix) => Ord (Array M ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance Index ix => Construct M ix e where
  getComp = mComp
  {-# INLINE getComp #-}

  setComp c arr = arr { mComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray !c !sz f = MArray c sz (V.unsafeIndex (makeBoxedVector sz f))
  {-# INLINE unsafeMakeArray #-}

-- | Create a boxed from usual size and index to element function
makeBoxedVector :: Index ix => ix -> (ix -> a) -> V.Vector a
makeBoxedVector !sz f = V.generate (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE makeBoxedVector #-}


-- | /O(1)/ - Conversion of `Manifest` arrays to `M` representation.
toManifest :: Manifest r ix e => Array r ix e -> Array M ix e
toManifest !arr = MArray (getComp arr) (size arr) (unsafeLinearIndexM arr)
{-# INLINE toManifest #-}


-- | Row-major sequential folding over a Manifest array.
instance Index ix => Foldable (Array M ix) where
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
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
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


instance Index ix => Size M ix e where
  size = mSize
  {-# INLINE size #-}

  unsafeResize !sz !arr = arr { mSize = sz }
  {-# INLINE unsafeResize #-}

  unsafeExtract !sIx !newSz !arr =
    MArray (getComp arr) newSz $ \ i ->
      unsafeIndex arr (liftIndex2 (+) (fromLinearIndex newSz i) sIx)
  {-# INLINE unsafeExtract #-}



instance {-# OVERLAPPING #-} Slice M Ix1 e where
  unsafeSlice arr i _ _ = Just (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         ) =>
         Slice M ix e where
  unsafeSlice arr start cutSz dim = do
    newSz <- dropDim cutSz dim
    return $ unsafeResize newSz (unsafeExtract start cutSz arr)
  {-# INLINE unsafeSlice #-}

instance {-# OVERLAPPING #-} OuterSlice M Ix1 e where
  unsafeOuterSlice !arr = unsafeIndex arr
  {-# INLINE unsafeOuterSlice #-}

instance (Elt M ix e ~ Array M (Lower ix) e, Index ix, Index (Lower ix)) => OuterSlice M ix e where
  unsafeOuterSlice !arr !i =
    MArray (getComp arr) (tailDim (size arr)) (unsafeLinearIndex arr . (+ kStart))
    where
      !kStart = toLinearIndex (size arr) (consDim i (zeroIndex :: Lower ix))
  {-# INLINE unsafeOuterSlice #-}

instance {-# OVERLAPPING #-} InnerSlice M Ix1 e where
  unsafeInnerSlice !arr _ = unsafeIndex arr
  {-# INLINE unsafeInnerSlice #-}

instance (Elt M ix e ~ Array M (Lower ix) e, Index ix, Index (Lower ix)) => InnerSlice M ix e where
  unsafeInnerSlice !arr (szL, m) !i =
    MArray (getComp arr) szL (\k -> unsafeLinearIndex arr (k * m + kStart))
    where
      !kStart = toLinearIndex (size arr) (snocDim (zeroIndex :: Lower ix) i)
  {-# INLINE unsafeInnerSlice #-}


instance Index ix => Load M ix e where
  loadArray numWorkers' scheduleWork' (MArray _ sz f) =
    splitLinearlyWith_ numWorkers' scheduleWork' (totalElem sz) f
  {-# INLINE loadArray #-}
instance Index ix => StrideLoad M ix e


loadMutableS :: (Load r' ix e, Mutable r ix e) =>
                Array r' ix e -> Array r ix e
loadMutableS !arr =
  runST $ do
    mArr <- unsafeNew (size arr)
    loadArray 1 id arr (unsafeLinearWrite mArr)
    unsafeFreeze Seq mArr
{-# INLINE loadMutableS #-}

loadMutableOnP :: (Load r' ix e, Mutable r ix e) =>
                 [Int] -> Array r' ix e -> IO (Array r ix e)
loadMutableOnP wids !arr = do
  mArr <- unsafeNew (size arr)
  withScheduler_ wids $ \scheduler ->
    loadArray (numWorkers scheduler) (scheduleWork scheduler) arr (unsafeLinearWrite mArr)
  unsafeFreeze (ParOn wids) mArr
{-# INLINE loadMutableOnP #-}


-- | Ensure that Array is computed, i.e. represented with concrete elements in memory, hence is the
-- `Mutable` type class restriction. Use `setComp` if you'd like to change computation strategy
-- before calling @compute@
compute :: (Load r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
compute !arr = unsafePerformIO $ do
  mArr <- unsafeNew (size arr)
  let !comp = getComp arr
      !wIds = case comp of
               Seq -> [1]
               ParOn caps -> caps
  withScheduler_ wIds $ \scheduler ->
    -- case getComp arr of
    -- Seq        -> loadArray 1 id arr (unsafeLinearWrite mArr)
    -- ParOn wids -> withScheduler_ wids $ \scheduler ->
      loadArray (numWorkers scheduler) (scheduleWork scheduler) arr (unsafeLinearWrite mArr)
  unsafeFreeze comp mArr
{-# INLINE compute #-}

-- | Just as `compute`, but let's you supply resulting representation type as an argument.
--
-- ====__Examples__
--
-- >>> computeAs P $ range Seq 0 10
-- (Array P Seq (10)
--   [ 0,1,2,3,4,5,6,7,8,9 ])
--
computeAs :: (Load r' ix e, Mutable r ix e) => r -> Array r' ix e -> Array r ix e
computeAs _ = compute
{-# INLINE computeAs #-}


-- | Same as `compute` and `computeAs`, but let's you supply resulting representation type as a proxy
-- argument.
--
-- @since 0.1.1
--
-- ====__Examples__
--
-- Useful for cases when representation constructor isn't available for some reason:
--
-- >>> computeProxy (Nothing :: Maybe P) $ range Seq 0 10
-- (Array P Seq (10)
--   [ 0,1,2,3,4,5,6,7,8,9 ])
--
computeProxy :: (Load r' ix e, Mutable r ix e) => proxy r -> Array r' ix e -> Array r ix e
computeProxy _ = compute
{-# INLINE computeProxy #-}


-- | Compute an Array while loading the results into the supplied mutable target array. Sizes of
-- both arrays must agree, otherwise error.
--
-- @since 0.1.3
computeInto ::
     (Load r' ix e, Mutable r ix e)
  => MArray RealWorld r ix e -- ^ Target Array
  -> Array r' ix e -- ^ Array to load
  -> IO ()
computeInto !mArr !arr = do
  unless (msize mArr == size arr) $ errorSizeMismatch "computeInto" (msize mArr) (size arr)
  case getComp arr of
    Seq -> loadArray 1 id arr (unsafeLinearWrite mArr)
    ParOn wids ->
      withScheduler_ wids $ \scheduler ->
        loadArray (numWorkers scheduler) (scheduleWork scheduler) arr (unsafeLinearWrite mArr)
{-# INLINE computeInto #-}


-- | This is just like `compute`, but can be applied to `Source` arrays and will be a noop if
-- resulting type is the same as the input.
computeSource :: forall r' r ix e . (Source r' ix e, Mutable r ix e)
              => Array r' ix e -> Array r ix e
computeSource arr =
  maybe (compute $ delay arr) (\Refl -> arr) (eqT :: Maybe (r' :~: r))
{-# INLINE computeSource #-}


-- | /O(n)/ - Make an exact immutable copy of an Array.
clone :: Mutable r ix e => Array r ix e -> Array r ix e
clone = compute . toManifest
{-# INLINE clone #-}


-- | /O(1)/ - Cast over Array representation
gcastArr :: forall r' r ix e. (Typeable r, Typeable r')
       => Array r' ix e -> Maybe (Array r ix e)
gcastArr arr = fmap (\Refl -> arr) (eqT :: Maybe (r :~: r'))


-- | /O(n)/ - conversion between manifest types, except when source and result arrays
-- are of the same representation, in which case it is an /O(1)/ operation.
convert :: (Manifest r' ix e, Mutable r ix e)
        => Array r' ix e -> Array r ix e
convert arr =
  fromMaybe (compute $ toManifest arr) (gcastArr arr)
{-# INLINE convert #-}

-- | Same as `convert`, but let's you supply resulting representation type as an argument.
convertAs :: (Manifest r' ix e, Mutable r ix e)
          => r -> Array r' ix e -> Array r ix e
convertAs _ = convert
{-# INLINE convertAs #-}


-- | Same as `convert` and `convertAs`, but let's you supply resulting representation type as a
-- proxy argument.
--
-- @since 0.1.1
--
convertProxy :: (Manifest r' ix e, Mutable r ix e)
             => proxy r -> Array r' ix e -> Array r ix e
convertProxy _ = convert
{-# INLINE convertProxy #-}

sequenceOnP :: (Source r1 ix (IO e), Mutable r ix e) =>
               [Int] -> Array r1 ix (IO e) -> IO (Array r ix e)
sequenceOnP wIds !arr = do
  resArrM <- unsafeNew (size arr)
  withScheduler_ wIds $ \scheduler ->
    flip imapM_ arr $ \ !ix action ->
      scheduleWork scheduler $ action >>= unsafeWrite resArrM ix
  unsafeFreeze (getComp arr) resArrM
{-# INLINE sequenceOnP #-}


sequenceP :: (Source r1 ix (IO e), Mutable r ix e) => Array r1 ix (IO e) -> IO (Array r ix e)
sequenceP = sequenceOnP []
{-# INLINE sequenceP #-}


-- | Convert a ragged array into a usual rectangular shaped one.
fromRaggedArray :: (Ragged r' ix e, Mutable r ix e) =>
                   Array r' ix e -> Either ShapeError (Array r ix e)
fromRaggedArray arr =
  unsafePerformIO $ do
    let sz = edgeSize arr
    mArr <- unsafeNew sz
    let loadWith using = loadRagged using (unsafeLinearWrite mArr) 0 (totalElem sz) sz arr
    try $
      case getComp arr of
        c -> do
          loadWith id
          unsafeFreeze c mArr
       -- Seq -> do
       --   loadWith id
       --   unsafeFreeze Seq mArr
       -- pComp@(ParOn ss) -> do
       --   withScheduler_ ss (loadWith . scheduleWork)
       --   unsafeFreeze pComp mArr
{-# INLINE fromRaggedArray #-}

-- | Same as `fromRaggedArray`, but will throw an error if its shape is not
-- rectangular.
fromRaggedArray' :: (Ragged r' ix e, Mutable r ix e) =>
                    Array r' ix e -> Array r ix e
fromRaggedArray' arr =
  case fromRaggedArray arr of
    Left RowTooShortError -> error "Not enough elements in a row"
    Left RowTooLongError  -> error "Too many elements in a row"
    Right resArr          -> resArr
{-# INLINE fromRaggedArray' #-}




-- | Same as `compute`, but with `Stride`.
computeWithStride ::
     (StrideLoad r' ix e, Mutable r ix e) => Stride ix -> Array r' ix e -> Array r ix e
computeWithStride stride !arr =
  unsafePerformIO $ do
    let sz = strideSize stride (size arr)
        comp = getComp arr
    mArr <- unsafeNew sz
    case comp of
      Seq -> loadArrayWithStride 1 id stride sz arr (unsafeLinearWrite mArr)
      ParOn wIds ->
        withScheduler_ wIds $ \scheduler ->
          loadArrayWithStride
            (numWorkers scheduler)
            (scheduleWork scheduler)
            stride
            sz
            arr
            (unsafeLinearWrite mArr)
    -- -- Alternative way to run computation sequentially: decreaseas compile time
    -- let wIds = case comp of
    --              Seq -> [1]
    --              ParOn caps -> caps
    -- withScheduler_ wIds $ \scheduler ->
    --       loadArrayWithStride
    --         (numWorkers scheduler)
    --         (scheduleWork scheduler)
    --         stride
    --         sz
    --         arr
    --         (unsafeLinearRead mArr)
    --         (unsafeLinearWrite mArr)
    unsafeFreeze comp mArr
{-# INLINE computeWithStride #-}


-- | Same as `computeWithStride`, but with ability to specify resulting array representation.
computeWithStrideAs ::
     (StrideLoad r' ix e, Mutable r ix e) => r -> Stride ix -> Array r' ix e -> Array r ix e
computeWithStrideAs _ = computeWithStride
{-# INLINE computeWithStrideAs #-}
