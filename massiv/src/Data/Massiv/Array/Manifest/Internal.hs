{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
  , computeSource
  , clone
  , convert
  , convertAs
  , gcastArr
  , loadMutableS
  , loadMutableOnP
  , sequenceP
  , sequenceOnP
  , fromRaggedArray
  , fromRaggedArray'
  ) where

import           Control.Exception                  (try)
import           Control.Monad.ST                   (runST)
import           Data.Foldable                      (Foldable (..))
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Ops.Fold         as M
import           Data.Massiv.Array.Ops.Map          (iforM_)
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           Data.Massiv.Core.Scheduler
import           Data.Maybe                         (fromMaybe)
import           Data.Typeable
import qualified Data.Vector                        as V
import           GHC.Base                           (build)
import           System.IO.Unsafe                   (unsafePerformIO)


-- | General Manifest representation
data M

data instance Array M ix e = MArray { mComp :: !Comp
                                    , mSize :: !ix
                                    , mUnsafeLinearIndex :: Int -> e }
type instance EltRepr M ix = M

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
toManifest !arr = MArray (getComp arr) (size arr) (unsafeLinearIndexM arr) where
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
  unsafeLinearIndex = mUnsafeLinearIndex
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Manifest M ix e where

  unsafeLinearIndexM = mUnsafeLinearIndex
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
  unsafeInnerSlice !arr !(szL, m) !i =
    MArray (getComp arr) szL (\k -> unsafeLinearIndex arr (k * m + kStart))
    where
      !kStart = toLinearIndex (size arr) (snocDim (zeroIndex :: Lower ix) i)
  {-# INLINE unsafeInnerSlice #-}


instance Index ix => Load M ix e where
  loadS (MArray _ sz f) _ uWrite =
    iterM_ 0 (totalElem sz) 1 (<) $ \ !i ->
      uWrite i (f i)
  {-# INLINE loadS #-}
  loadP wIds (MArray _ sz f) _ uWrite = do
    divideWork_ wIds (totalElem sz) $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        scheduleWork scheduler $
        iterM_ start (start + chunkLength) 1 (<) $ \ !i ->
          uWrite i (f i)
      scheduleWork scheduler $
        iterM_ slackStart totalLength 1 (<) $ \ !i ->
          uWrite i (f i)
  {-# INLINE loadP #-}



loadMutableS :: (Load r' ix e, Mutable r ix e) =>
                Array r' ix e -> Array r ix e
loadMutableS !arr =
  runST $ do
    mArr <- unsafeNew (size arr)
    loadS arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
    unsafeFreeze Seq mArr
{-# INLINE loadMutableS #-}

loadMutableOnP :: (Load r' ix e, Mutable r ix e) =>
                 [Int] -> Array r' ix e -> IO (Array r ix e)
loadMutableOnP wIds !arr = do
  mArr <- unsafeNew (size arr)
  loadP wIds arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
  unsafeFreeze (ParOn wIds) mArr
{-# INLINE loadMutableOnP #-}

-- | Ensure that Array is computed, i.e. represented with concrete elements in memory, hence is the
-- `Mutable` type class restriction. Use `setComp` if you'd like to change computation strategy
-- before calling @compute@
compute :: (Load r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
compute !arr =
  case getComp arr of
    Seq        -> loadMutableS arr
    ParOn wIds -> unsafePerformIO $ loadMutableOnP wIds arr
{-# INLINE compute #-}

-- | Just as `compute`, but let's you supply resulting representation type as an argument.
computeAs :: (Load r' ix e, Mutable r ix e) => r -> Array r' ix e -> Array r ix e
computeAs _ = compute
{-# INLINE computeAs #-}


-- | This is just like `compute`, but can be applied to `Source` arrays and will be a noop if
-- resulting type is the same as the input.
computeSource :: forall r' r ix e . (Source r' ix e, Mutable r ix e)
              => Array r' ix e -> Array r ix e
computeSource arr =
  fromMaybe (compute $ delay arr) $ fmap (\Refl -> arr) (eqT :: Maybe (r' :~: r))
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
convertAs :: (Mutable r' ix e, Mutable r ix e, Typeable ix, Typeable e)
          => r -> Array r' ix e -> Array r ix e
convertAs _ = convert
{-# INLINE convertAs #-}


sequenceOnP :: (Source r1 ix (IO e), Mutable r ix e) =>
               [Int] -> Array r1 ix (IO e) -> IO (Array r ix e)
sequenceOnP wIds !arr = do
  resArrM <- unsafeNew (size arr)
  withScheduler_ wIds $ \scheduler ->
    iforM_ arr $ \ !ix action ->
      scheduleWork scheduler $ action >>= unsafeWrite resArrM ix
  unsafeFreeze (getComp arr) resArrM
{-# INLINE sequenceOnP #-}


sequenceP :: (Source r1 ix (IO e), Mutable r ix e) => Array r1 ix (IO e) -> IO (Array r ix e)
sequenceP = sequenceOnP []
{-# INLINE sequenceP #-}





-- sequenceOnP' :: (NFData e, Source r1 ix (IO e), Mutable r ix e) =>
--                [Int] -> Array r1 ix (IO e) -> IO (Array r ix e)
-- sequenceOnP' wIds !arr = do
--   resArrM <- unsafeNew (size arr)
--   scheduler <- makeScheduler wIds
--   iforM_ arr $ \ !ix action ->
--     submitRequest scheduler $ JobRequest $ do
--       res <- action
--       res `deepseq` unsafeWrite resArrM ix res
--   waitTillDone scheduler
--   unsafeFreeze resArrM
-- {-# INLINE sequenceOnP' #-}


-- sequenceP' :: (NFData e, Source r1 ix (IO e), Mutable r ix e)
--            => Array r1 ix (IO e) -> IO (Array r ix e)
-- sequenceP' = sequenceOnP' []
-- {-# INLINE sequenceP' #-}

-- | Convert a ragged array into a usual rectangular shaped one.
fromRaggedArray :: (Ragged r' ix e, Mutable r ix e) =>
                   Array r' ix e -> Either ShapeError (Array r ix e)
fromRaggedArray arr = unsafePerformIO $ do
  let sz = edgeSize arr
  mArr <- unsafeNew sz
  let loadWith using =
        loadRagged using (unsafeLinearWrite mArr) 0 (totalElem sz) (tailDim sz) arr
  try $ case getComp arr of
          Seq -> loadWith id >> unsafeFreeze (getComp arr) mArr
          ParOn ss -> do
            withScheduler_ ss (loadWith . scheduleWork)
            unsafeFreeze (getComp arr) mArr
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

