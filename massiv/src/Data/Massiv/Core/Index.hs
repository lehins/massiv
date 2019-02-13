{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators   #-}
-- |
-- Module      : Data.Massiv.Core.Index
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index
  ( Ix0(..)
  , type Ix1
  , pattern Ix1
  , type Ix2(Ix2, (:.))
  , IxN((:>), Ix3, Ix4, Ix5)
  , type Ix3
  , type Ix4
  , type Ix5
  , Ix
  -- ** Size
  , type Sz1
  , type Sz2
  , type Sz3
  , type Sz4
  , type Sz5
  , Sz(Sz, Sz1, Sz2, Sz3, Sz4, Sz5)
  , unSz
  , zeroSz
  , oneSz
  , consSz
  , unconsSz
  , snocSz
  , unsnocSz
  , setSzM
  , insertSzM
  , pullOutSzM
  , Dim(..)
  , Dimension(Dim1, Dim2, Dim3, Dim4, Dim5, DimN)
  , IsIndexDimension
  , Lower
  , Index(..)
  , Stride(Stride)
  , unStride
  , toLinearIndexStride
  , strideStart
  , strideSize
  , oneStride
  , Border(..)
  , handleBorderIndex
  , zeroIndex
  , oneIndex
  , isNonEmpty
  , headDim
  , tailDim
  , lastDim
  , initDim
  , getDim'
  , getDim
  , setDim'
  , setDim
  , dropDimM
  , dropDim'
  , dropDim
  , pullOutDim'
  , pullOutDim
  , insertDim'
  , insertDim
  , fromDimension
  , getDimension
  , setDimension
  , dropDimension
  , pullOutDimension
  , insertDimension
  -- * Iterators
  , iter
  , iterLinearM
  , iterLinearM_
  , module Data.Massiv.Core.Iterator
  , module Data.Massiv.Core.Index.Tuple
  -- * Exceptions
  , IndexException(..)
  , SizeException(..)
  , ShapeException(..)
  ) where

import           Control.Exception               (throw)
import           Control.DeepSeq
import           Control.Monad.Catch             (MonadThrow(..))
import           Data.Functor.Identity           (runIdentity)
import           Data.Massiv.Core.Index.Internal
import           Data.Massiv.Core.Index.Tuple
import           Data.Massiv.Core.Index.Ix
import           Data.Massiv.Core.Index.Stride
import           Data.Massiv.Core.Iterator
import           GHC.TypeLits


-- | Approach to be used near the borders during various transformations.
-- Whenever a function needs information not only about an element of interest, but
-- also about it's neighbors, it will go out of bounds near the array edges,
-- hence is this set of approaches that specify how to handle such situation.
data Border e =
  Fill e    -- ^ Fill in a constant element.
              --
              -- @
              --            outside |  Array  | outside
              -- ('Fill' 0) : 0 0 0 0 | 1 2 3 4 | 0 0 0 0
              -- @
              --
  | Wrap      -- ^ Wrap around from the opposite border of the array.
              --
              -- @
              --            outside |  Array  | outside
              -- 'Wrap' :     1 2 3 4 | 1 2 3 4 | 1 2 3 4
              -- @
              --
  | Edge      -- ^ Replicate the element at the edge.
              --
              -- @
              --            outside |  Array  | outside
              -- 'Edge' :     1 1 1 1 | 1 2 3 4 | 4 4 4 4
              -- @
              --
  | Reflect   -- ^ Mirror like reflection.
              --
              -- @
              --            outside |  Array  | outside
              -- 'Reflect' :  4 3 2 1 | 1 2 3 4 | 4 3 2 1
              -- @
              --
  | Continue  -- ^ Also mirror like reflection, but without repeating the edge element.
              --
              -- @
              --            outside |  Array  | outside
              -- 'Continue' : 1 4 3 2 | 1 2 3 4 | 3 2 1 4
              -- @
              --
  deriving (Eq, Show)

instance NFData e => NFData (Border e) where
  rnf b = case b of
            Fill e   -> rnf e
            Wrap     -> ()
            Edge     -> ()
            Reflect  -> ()
            Continue -> ()


-- | Apply a border resolution technique to an index
--
-- ==== __Examples__
--
-- >>> handleBorderIndex (Fill 100) (Sz (2 :. 3)) id (2 :. 3)
-- 100 :. 100
-- >>> handleBorderIndex Wrap (Sz (2 :. 3)) id (2 :. 3)
-- 0 :. 0
-- >>> handleBorderIndex Edge (Sz (2 :. 3)) id (2 :. 3)
-- 1 :. 2
--
-- @since 0.1.0
handleBorderIndex ::
     Index ix
  => Border e -- ^ Broder resolution technique
  -> Sz ix -- ^ Size
  -> (ix -> e) -- ^ Index function that produces an element
  -> ix -- ^ Index
  -> e
handleBorderIndex border !sz getVal !ix =
  case border of
    Fill val -> if isSafeIndex sz ix then getVal ix else val
    Wrap     -> getVal (repairIndex sz ix wrap wrap)
    Edge     -> getVal (repairIndex sz ix (const (const 0)) (\ (SafeSz k) _ -> k - 1))
    Reflect  -> getVal (repairIndex sz ix (\ (SafeSz k) !i -> (abs i - 1) `mod` k)
                        (\ (SafeSz k) !i -> (-i - 1) `mod` k))
    Continue -> getVal (repairIndex sz ix (\ (SafeSz k) !i -> abs i `mod` k)
                        (\ (SafeSz k) !i -> (-i - 2) `mod` k))

  where wrap (SafeSz k) i = i `mod` k
        {-# INLINE [1] wrap #-}
{-# INLINE [1] handleBorderIndex #-}

-- | Index with all zeros
--
-- ==== __Examples__
--
--
-- @since 0.1.0
zeroIndex :: Index ix => ix
zeroIndex = pureIndex 0
{-# INLINE [1] zeroIndex #-}

-- | Index with all ones
--
-- @since 0.3.0
oneIndex :: Index ix => ix
oneIndex = pureIndex 1
{-# INLINE [1] oneIndex #-}

-- | Checks whether array with this size can hold at least one element.
--
-- ==== __Examples__
--
--
-- @since 0.1.0
isNonEmpty :: Index ix => Sz ix -> Bool
isNonEmpty !sz = isSafeIndex sz zeroIndex
{-# INLINE [1] isNonEmpty #-}
-- TODO: benchmark against (also adjust `isEmpty` with fastest):
-- - foldlIndex (*) 1 (unSz sz) /= 0
-- - foldlIndex ((&&) . (==0)) True (unSz sz)
-- - totalElem sz == 0

-- | Get the outmost dimension of the index.
--
-- ==== __Examples__
--
-- >>> headDim (2 :> 3 :> 4 :. 5)
-- 2
--
-- @since 0.1.0
headDim :: Index ix => ix -> Int
headDim = fst . unconsDim
{-# INLINE [1] headDim #-}

-- | Drop the outmost dimension from the index
--
-- ==== __Examples__
--
-- >>> tailDim (2 :> 3 :> 4 :. 5)
-- 3 :> 4 :. 5
--
-- @since 0.1.0
tailDim :: Index ix => ix -> Lower ix
tailDim = snd . unconsDim
{-# INLINE [1] tailDim #-}

-- | Get the innermost dimension from the index
--
-- ==== __Examples__
--
-- >>> lastDim (2 :> 3 :> 4 :. 5)
-- 5
--
-- @since 0.1.0
lastDim :: Index ix => ix -> Int
lastDim = snd . unsnocDim
{-# INLINE [1] lastDim #-}

-- | Drop the innermost dimension from the index
--
-- ==== __Examples__
--
-- >>> initDim (2 :> 3 :> 4 :. 5)
-- 2 :> 3 :. 4
--
-- @since 0.1.0
initDim :: Index ix => ix -> Lower ix
initDim = fst . unsnocDim
{-# INLINE [1] initDim #-}

-- | Change the value of a specific dimension within the index. Throws `IndexException`. See
-- `setDimM` for a safer version and `setDimension` for a type safe version.
--
-- ==== __Examples__
--
-- >>> setDim' (2 :> 3 :> 4 :. 5) 3 10
-- 2 :> 10 :> 4 :. 5
--
-- @since 0.2.4
setDim' :: Index ix => ix -> Dim -> Int -> ix
setDim' ix dim = either throw id . setDimM ix dim
{-# INLINE [1] setDim' #-}

-- | See `setDimM`
--
-- @since 0.2.4
setDim :: Index ix => ix -> Dim -> Int -> Maybe ix
setDim = setDimM
{-# INLINE [1] setDim #-}
{-# DEPRECATED setDim "In favor of more general `setDimM`" #-}

-- | Change the value from a specific dimension within the index. Throws `IndexException`. See
-- `getDimM` for a safer version and `getDimension` for a type safe version.
--
-- ==== __Examples__
--
-- >>> getDim' (2 :> 3 :> 4 :. 5) 3
-- 3
-- >>> getDim' (2 :> 3 :> 4 :. 5) 0
-- *** Exception: IndexDimensionException: (Dim 0) for 3 :> 4 :. 5
--
-- @since 0.2.4
getDim' :: Index ix => ix -> Dim -> Int
getDim' ix = either throw id . getDimM ix
{-# INLINE [1] getDim' #-}

-- | See `getDimM`
--
-- @since 0.2.4
getDim :: Index ix => ix -> Dim -> Maybe Int
getDim = getDimM
{-# INLINE [1] getDim #-}
{-# DEPRECATED getDim "In favor of more general `getDimM`" #-}

-- | Remove a dimension from the index.
--
-- ==== __Examples__
--
-- λ> dropDimM (2 :> 3 :> 4 :. 5) 3 :: Maybe Ix3
-- Just (2 :> 4 :. 5)
-- λ> dropDimM (2 :> 3 :> 4 :. 5) 6 :: Maybe Ix3
-- Nothing
--
-- @since 0.3.0
dropDimM :: (MonadThrow m, Index ix) => ix -> Dim -> m (Lower ix)
dropDimM ix = fmap snd . pullOutDimM ix
{-# INLINE [1] dropDimM #-}

-- | See `dropDimM`
--
-- @since 0.1.0
dropDim :: Index ix => ix -> Dim -> Maybe (Lower ix)
dropDim = dropDimM
{-# INLINE [1] dropDim #-}
{-# DEPRECATED dropDim "In favor of more general `dropDimM`" #-}

-- | Remove a dimension from the index.
--
-- ==== __Examples__
--
-- >>> dropDim' (2 :> 3 :> 4 :. 5) 3
-- 2 :> 4 :. 5
-- >>> dropDim' (2 :> 3 :> 4 :. 5) 6
-- *** Exception: IndexDimensionException: (Dim 6) for 3 :> 4 :. 5
--
-- @since 0.2.4
dropDim' :: Index ix => ix -> Dim -> Lower ix
dropDim' ix = either throw id . dropDimM ix
{-# INLINE [1] dropDim' #-}

-- | Lower the dimension of the index by pulling the specified dimension. Throws `IndexException`. See
-- `pullOutDimM` for a safer version and `pullOutDimension` for a type safe version.
--
-- ==== __Examples__
--
-- λ> pullOutDim' (2 :> 3 :> 4 :. 5) 3
-- (3,2 :> 4 :. 5)
--
-- @since 0.2.4
pullOutDim' :: Index ix => ix -> Dim -> (Int, Lower ix)
pullOutDim' ix = either throw id . pullOutDimM ix
{-# INLINE [1] pullOutDim' #-}

-- | See `pullOutDimM`
--
-- @since 0.2.4
pullOutDim :: Index ix => ix -> Dim -> Maybe (Int, Lower ix)
pullOutDim = pullOutDimM
{-# INLINE [1] pullOutDim #-}
{-# DEPRECATED pullOutDim "In favor of more general `pullOutDimM`" #-}

-- | Raise the dimension of the index by inserting one in the specified dimension. Throws
-- `IndexException`. See `insertDimM` for a safer version and `insertDimension` for a type safe
-- version.
--
-- ==== __Examples__
--
-- >>> insertDim' (2 :> 3 :> 4 :. 5) 3 10 :: Ix5
-- 2 :> 3 :> 10 :> 4 :. 5
-- >>> insertDim' (2 :> 3 :> 4 :. 5) 11 10 :: Ix5
-- *** Exception: IndexDimensionException: (Dim 11) for 4 :. 5
--
-- @since 0.2.4
insertDim' :: Index ix => Lower ix -> Dim -> Int -> ix
insertDim' ix dim = either throw id . insertDimM ix dim
{-# INLINE [1] insertDim' #-}

-- | See `insertDimM`
--
-- @since 0.2.4
insertDim :: Index ix => Lower ix -> Dim -> Int -> Maybe ix
insertDim = insertDimM
{-# INLINE [1] insertDim #-}
{-# DEPRECATED insertDim "In favor of more general `insertDimM`" #-}

-- | Get the value level `Dim` from the type level equivalent.
--
-- ==== __Examples__
--
-- >>> fromDimension Dim4
-- (Dim 4)
-- >>> :set -XDataKinds
-- >>> fromDimension (DimN :: Dimension 10)
-- (Dim 10)
--
-- @since 0.2.4
fromDimension :: KnownNat n => Dimension n -> Dim
fromDimension = fromIntegral . natVal
{-# INLINE [1] fromDimension #-}

-- | Type safe way to set value of index at a particular dimension.
--
-- ==== __Examples__
--
-- >>> setDimension (2 :> 3 :> 4 :. 5) Dim4 10
-- 10 :> 3 :> 4 :. 5
--
-- @since 0.2.4
setDimension :: IsIndexDimension ix n => ix -> Dimension n -> Int -> ix
setDimension ix d = setDim' ix (fromDimension d)
{-# INLINE [1] setDimension #-}

-- | Type safe way to extract value of index at a particular dimension.
--
-- ==== __Examples__
--
-- >>> getDimension (2 :> 3 :> 4 :. 5) Dim2
-- 4
--
-- @since 0.2.4
getDimension :: IsIndexDimension ix n => ix -> Dimension n -> Int
getDimension ix d = getDim' ix (fromDimension d)
{-# INLINE [1] getDimension #-}


-- | Type safe way of dropping a particular dimension, thus lowering index
-- dimensionality.
--
-- ==== __Examples__
--
-- >>> dropDimension (2 :> 3 :> 4 :. 5) Dim2
-- 2 :> 3 :. 5
--
-- @since 0.2.4
dropDimension :: IsIndexDimension ix n => ix -> Dimension n -> Lower ix
dropDimension ix d = dropDim' ix (fromDimension d)
{-# INLINE [1] dropDimension #-}

-- | Type safe way of pulling out a particular dimension, thus lowering index
-- dimensionality and returning the value at specified dimension.
--
-- ==== __Examples__
--
-- >>> pullOutDimension (2 :> 3 :> 4 :. 5) Dim2
-- (4,2 :> 3 :. 5)
--
-- @since 0.2.4
pullOutDimension :: IsIndexDimension ix n => ix -> Dimension n -> (Int, Lower ix)
pullOutDimension ix d = pullOutDim' ix (fromDimension d)
{-# INLINE [1] pullOutDimension #-}

-- | Type safe way of inserting a particular dimension, thus raising index dimensionality.
--
-- ==== __Examples__
--
-- >>> insertDimension (2 :> 3 :> 4 :. 5) Dim5 10 :: Ix5
-- 10 :> 2 :> 3 :> 4 :. 5
-- >>> insertDimension (2 :> 3 :> 4 :. 5) Dim4 10 :: Ix5
-- 2 :> 10 :> 3 :> 4 :. 5
-- >>> insertDimension (2 :> 3 :> 4 :. 5) Dim3 10 :: Ix5
-- 2 :> 3 :> 10 :> 4 :. 5
-- >>> insertDimension (2 :> 3 :> 4 :. 5) Dim2 10 :: Ix5
-- 2 :> 3 :> 4 :> 10 :. 5
-- >>> insertDimension (2 :> 3 :> 4 :. 5) Dim1 10 :: Ix5
-- 2 :> 3 :> 4 :> 5 :. 10
--
-- @since 0.2.5
insertDimension :: IsIndexDimension ix n => Lower ix -> Dimension n -> Int -> ix
insertDimension ix d = insertDim' ix (fromDimension d)
{-# INLINE [1] insertDimension #-}

-- | Row-major iterator for the index. Same as `iterM`, but pure.
--
-- ==== __Examples__
--
-- >>> iter (Ix1 0) 1000 1 (<) 0 (+)
-- 499500
-- >>> iter (0 :. 0) (2 :. 3) oneIndex (<) 100 $ \ (i :. j) acc -> (acc + i) * (j + 1)
-- 3615
--
-- @since 0.1.0
iter :: Index ix
  => ix -- ^ Start index
  -> ix -- ^ End index
  -> ix -- ^ Increment
  -> (Int -> Int -> Bool) -- ^ Continuation confition
  -> a -- ^ Accumulator
  -> (ix -> a -> a) -- ^ Iterating function
  -> a
iter sIx eIx incIx cond acc f =
  runIdentity $ iterM sIx eIx incIx cond acc (\ix -> return . f ix)
{-# INLINE iter #-}


-- | Iterate over N-dimensional space linearly from start to end in row-major fashion with an
-- accumulator
--
-- ==== __Examples__
--
-- >>> sz = Sz2 3 4
-- >>> iterLinearM sz 0 3 1 (<) 100 $ \ k ix acc -> print (fromLinearIndex sz k == ix) >> pure (acc + k)
-- True
-- True
-- True
-- 103
--
-- @since 0.1.0
iterLinearM :: (Index ix, Monad m)
            => Sz ix -- ^ Size
            -> Int -- ^ Linear start
            -> Int -- ^ Linear end
            -> Int -- ^ Increment
            -> (Int -> Int -> Bool) -- ^ Continuation condition (continue if True)
            -> a -- ^ Accumulator
            -> (Int -> ix -> a -> m a)
            -> m a
iterLinearM !sz !k0 !k1 !inc cond !acc f =
  loopM k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 -> f i (fromLinearIndex sz i) acc0
{-# INLINE iterLinearM #-}

-- | Same as `iterLinearM`, except without an accumulator.
--
-- ==== __Examples__
--
-- >>> sz = Sz2 3 4
-- >>> iterLinearM_ sz 0 3 1 (<) $ \ k ix -> print (toLinearIndex sz ix == k)
-- True
-- True
-- True
--
-- @since 0.1.0
iterLinearM_ :: (Index ix, Monad m) =>
                Sz ix -- ^ Size
             -> Int -- ^ Start
             -> Int -- ^ End
             -> Int -- ^ Increment
             -> (Int -> Int -> Bool) -- ^ Continuation condition
             -> (Int -> ix -> m ()) -- ^ Monadic action that takes index in both forms
             -> m ()
iterLinearM_ sz !k0 !k1 !inc cond f =
  loopM_ k0 (`cond` k1) (+ inc) $ \ !i -> f i (fromLinearIndex sz i)
{-# INLINE iterLinearM_ #-}
