{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
-- |
-- Module      : Data.Massiv.Core.Index
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
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
  , HighIxN
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
  , liftSz
  , liftSz2
  , consSz
  , unconsSz
  , snocSz
  , unsnocSz
  , setSzM
  , insertSzM
  , pullOutSzM
  , toLinearSz
  , mkSzM
  -- ** Dimension
  , Dim(..)
  , Dimension(Dim1, Dim2, Dim3, Dim4, Dim5, DimN)
  , IsIndexDimension
  , IsDimValid
  , ReportInvalidDim
  -- ** Stride
  , Stride(Stride)
  , unStride
  , toLinearIndexStride
  , strideStart
  , strideSize
  , oneStride
  -- ** Border
  , Border(..)
  , handleBorderIndex
  -- ** Index functions
  , Lower
  , Index(..)
  , zeroIndex
  , oneIndex
  , isZeroSz
  , isNotZeroSz
  , headDim
  , tailDim
  , lastDim
  , initDim
  , getDim'
  , setDim'
  , modifyDim'
  , dropDimM
  , dropDim'
  , pullOutDim'
  , insertDim'
  , fromDimension
  , getDimension
  , setDimension
  , modifyDimension
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
  , guardNumberOfElements
  , indexWith
  ) where

import Primal.Monad.Raises
import Primal.Eval
import Data.Coerce
import Data.Functor.Identity (runIdentity)
import Data.Massiv.Core.Exception
import Data.Massiv.Core.Index.Internal
import Data.Massiv.Core.Index.Ix
import Data.Massiv.Core.Index.Stride
import Data.Massiv.Core.Index.Tuple
import Data.Massiv.Core.Iterator
import GHC.TypeLits

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
-- >>> zeroIndex :: Ix4
-- 0 :> 0 :> 0 :. 0
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

-- | Checks whether size can hold at least one element.
--
-- ==== __Examples__
--
-- >>> isNotZeroSz (Sz3 1 0 2)
-- False
--
-- @since 1.0.0
isNotZeroSz :: Index ix => Sz ix -> Bool
isNotZeroSz !sz = isSafeIndex sz zeroIndex
{-# INLINE [1] isNotZeroSz #-}
-- TODO: benchmark against (also adjust `isEmpty` with fastest):
-- - foldlIndex (*) 1 (unSz sz) /= 0
-- - foldlIndex (\a x -> a && x /= 0) True (unSz sz)
-- - totalElem sz == 0

-- | Checks whether size can hold at least one element.
--
-- ==== __Examples__
--
-- >>> isZeroSz (Sz3 1 0 2)
-- True
--
-- @since 1.0.0
isZeroSz :: Index ix => Sz ix -> Bool
isZeroSz = not . isNotZeroSz
{-# INLINE [1] isZeroSz #-}


-- | Convert a size to a linear size.
--
-- @since 0.5.8
toLinearSz :: Index ix => Sz ix -> Sz1
toLinearSz = coerce . totalElem
{-# INLINE [1] toLinearSz #-}

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

-- | Change the value of a specific dimension within the index. See `setDimM` for a safer
-- version and `setDimension` for a type safe version.
--
-- ==== __Examples__
--
-- >>> setDim' (2 :> 3 :> 4 :. 5) 3 10
-- 2 :> 10 :> 4 :. 5
--
-- @since 0.2.4
setDim' :: (HasCallStack, Index ix) => ix -> Dim -> Int -> ix
setDim' ix dim = raiseLeftImprecise . setDimM ix dim
{-# INLINE [1] setDim' #-}

-- | Change the value from a specific dimension within the index. See
-- `getDimM` for a safer version and `getDimension` for a type safe version.
--
-- ==== __Examples__
--
-- >>> getDim' (2 :> 3 :> 4 :. 5) 3
-- 3
--
-- @since 0.2.4
getDim' :: (HasCallStack, Index ix) => ix -> Dim -> Int
getDim' ix = raiseLeftImprecise . getDimM ix
{-# INLINE [1] getDim' #-}

-- | Update the value of a specific dimension within the index. See
-- `modifyDimM` for a safer version and `modifyDimension` for a type safe version.
--
-- ==== __Examples__
--
-- >>> modifyDim' (2 :> 3 :> 4 :. 5) 2 (+ 10)
-- (4,2 :> 3 :> 14 :. 5)
--
-- @since 0.4.1
modifyDim' :: (HasCallStack, Index ix) => ix -> Dim -> (Int -> Int) -> (Int, ix)
modifyDim' ix dim = raiseLeftImprecise . modifyDimM ix dim
{-# INLINE [1] modifyDim' #-}

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
dropDimM :: (Raises m, Index ix) => ix -> Dim -> m (Lower ix)
dropDimM ix = fmap snd . pullOutDimM ix
{-# INLINE [1] dropDimM #-}

-- | Remove a dimension from the index.
--
-- ==== __Examples__
--
-- >>> dropDim' (2 :> 3 :> 4 :. 5) 3
-- 2 :> 4 :. 5
--
-- @since 0.2.4
dropDim' :: (HasCallStack, Index ix) => ix -> Dim -> Lower ix
dropDim' ix = raiseLeftImprecise . dropDimM ix
{-# INLINE [1] dropDim' #-}

-- | Lower the dimension of the index by pulling the specified dimension. See
-- `pullOutDimM` for a safer version and `pullOutDimension` for a type safe version.
--
-- ==== __Examples__
--
-- λ> pullOutDim' (2 :> 3 :> 4 :. 5) 3
-- (3,2 :> 4 :. 5)
--
-- @since 0.2.4
pullOutDim' :: (HasCallStack, Index ix) => ix -> Dim -> (Int, Lower ix)
pullOutDim' ix = raiseLeftImprecise . pullOutDimM ix
{-# INLINE [1] pullOutDim' #-}

-- | Raise the dimension of the index by inserting one in the specified dimension. See
-- `insertDimM` for a safer version and `insertDimension` for a type safe version.
--
-- ==== __Examples__
--
-- >>> insertDim' (2 :> 3 :> 4 :. 5) 3 10 :: Ix5
-- 2 :> 3 :> 10 :> 4 :. 5
--
-- @since 0.2.4
insertDim' :: (HasCallStack, Index ix) => Lower ix -> Dim -> Int -> ix
insertDim' ix dim = raiseLeftImprecise . insertDimM ix dim
{-# INLINE [1] insertDim' #-}

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
setDimension ix = setDim' ix . fromDimension
{-# INLINE [1] setDimension #-}

-- | Type safe way to set value of index at a particular dimension.
--
-- ==== __Examples__
--
-- >>> modifyDimension (2 :> 3 :> 4 :. 5) Dim3 (+ 2)
-- (3,2 :> 5 :> 4 :. 5)
--
-- @since 0.4.1
modifyDimension :: IsIndexDimension ix n => ix -> Dimension n -> (Int -> Int) -> (Int, ix)
modifyDimension ix = modifyDim' ix . fromDimension
{-# INLINE [1] modifyDimension #-}

-- | Type safe way to extract value of index at a particular dimension.
--
-- ==== __Examples__
--
-- >>> getDimension (2 :> 3 :> 4 :. 5) Dim2
-- 4
--
-- @since 0.2.4
getDimension :: IsIndexDimension ix n => ix -> Dimension n -> Int
getDimension ix = getDim' ix . fromDimension
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
dropDimension ix = dropDim' ix . fromDimension
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
pullOutDimension ix = pullOutDim' ix . fromDimension
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
insertDimension ix = insertDim' ix . fromDimension
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
  -> (Int -> Int -> Bool) -- ^ Continuation condition
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
            -> Int -- ^ Linear start (must be non-negative)
            -> Int -- ^ Linear end (must be less than or equal to @`totalElem` sz@)
            -> Int -- ^ Increment (must not be zero)
            -> (Int -> Int -> Bool) -- ^ Continuation condition (continue if @True@)
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
             -> Int -- ^ Start (must be non-negative)
             -> Int -- ^ End
             -> Int -- ^ Increment (must not be zero)
             -> (Int -> Int -> Bool) -- ^ Continuation condition (continue if @True@)
             -> (Int -> ix -> m ()) -- ^ Monadic action that takes index in both forms
             -> m ()
iterLinearM_ sz !k0 !k1 !inc cond f =
  loopM_ k0 (`cond` k1) (+ inc) $ \ !i -> f i (fromLinearIndex sz i)
{-# INLINE iterLinearM_ #-}


-- | This is used by @INDEX_CHECK@ macro and thus used whenever the @unsafe-checks@ cabal
-- flag is on.
--
-- @since 0.4.0
indexWith ::
     Index ix
  => String -- ^ Source file name, eg. __FILE__
  -> Int -- ^ Line number in th source file, eg. __LINE__
  -> String
  -> (arr -> Sz ix) -- ^ Get size of the array
  -> (arr -> ix -> e) -- ^ Indexing function
  -> arr -- ^ Array
  -> ix -- ^ Index
  -> e
indexWith fileName lineNo funName getSize f arr ix
  | isSafeIndex sz ix = f arr ix
  | otherwise = errorIx ("<" ++ fileName ++ ":" ++ show lineNo ++ "> " ++ funName) sz ix
  where
    sz = getSize arr

-- | Helper function for throwing out of bounds error. Used by `indexWith`
errorIx :: (Show ix, Show ix') => String -> ix -> ix' -> a
errorIx fName sz ix =
  error $
  fName ++
  ": Index out of bounds: (" ++ show ix ++ ") for Array of size: (" ++ show sz ++ ")"
{-# NOINLINE errorIx #-}

