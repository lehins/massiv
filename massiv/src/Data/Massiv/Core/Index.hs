{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators   #-}
-- |
-- Module      : Data.Massiv.Core.Index
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index
  ( module Data.Massiv.Core.Index.Ix
  , Stride
  , pattern Stride
  , unStride
  , toLinearIndexStride
  , strideStart
  , strideSize
  , oneStride
  , Border(..)
  , handleBorderIndex
  , module Data.Massiv.Core.Index.Class
  , zeroIndex
  , isSafeSize
  , isNonEmpty
  , headDim
  , tailDim
  , lastDim
  , initDim
  , getIndex'
  , setIndex'
  , getDim'
  , setDim'
  , dropDim'
  , pullOutDim'
  , insertDim'
  , fromDimension
  , getDimension
  , setDimension
  , dropDimension
  , pullOutDimension
  , insertDimension
  , iterLinearM
  , iterLinearM_
  , module Data.Massiv.Core.Iterator
  ) where

import           Control.DeepSeq
import           Data.Massiv.Core.Index.Class
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
handleBorderIndex ::
     Index ix
  => Border e -- ^ Broder resolution technique
  -> ix -- ^ Size
  -> (ix -> e) -- ^ Index function that produces an element
  -> ix -- ^ Index
  -> e
handleBorderIndex border !sz getVal !ix =
  case border of
    Fill val -> if isSafeIndex sz ix then getVal ix else val
    Wrap     -> getVal (repairIndex sz ix (flip mod) (flip mod))
    Edge     -> getVal (repairIndex sz ix (const (const 0)) (\ !k _ -> k - 1))
    Reflect  -> getVal (repairIndex sz ix (\ !k !i -> (abs i - 1) `mod` k)
                        (\ !k !i -> (-i - 1) `mod` k))
    Continue -> getVal (repairIndex sz ix (\ !k !i -> abs i `mod` k)
                        (\ !k !i -> (-i - 2) `mod` k))
{-# INLINE [1] handleBorderIndex #-}

-- | Index with all zeros
zeroIndex :: Index ix => ix
zeroIndex = pureIndex 0
{-# INLINE [1] zeroIndex #-}

-- | Checks whether the size is valid.
isSafeSize :: Index ix => ix -> Bool
isSafeSize = (zeroIndex >=)
{-# INLINE [1] isSafeSize #-}


-- | Checks whether array with this size can hold at least one element.
isNonEmpty :: Index ix => ix -> Bool
isNonEmpty !sz = isSafeIndex sz zeroIndex
{-# INLINE [1] isNonEmpty #-}


headDim :: Index ix => ix -> Int
headDim = fst . unconsDim
{-# INLINE [1] headDim #-}

tailDim :: Index ix => ix -> Lower ix
tailDim = snd . unconsDim
{-# INLINE [1] tailDim #-}

lastDim :: Index ix => ix -> Int
lastDim = snd . unsnocDim
{-# INLINE [1] lastDim #-}

initDim :: Index ix => ix -> Lower ix
initDim = fst . unsnocDim
{-# INLINE [1] initDim #-}



setDim' :: Index ix => ix -> Dim -> Int -> ix
setDim' ix dim i =
  case setDim ix dim i of
    Just ix' -> ix'
    Nothing  -> errorDim "setDim'" dim
{-# INLINE [1] setDim' #-}

getDim' :: Index ix => ix -> Dim -> Int
getDim' ix dim =
  case getDim ix dim of
    Just ix' -> ix'
    Nothing  -> errorDim "getDim'" dim
{-# INLINE [1] getDim' #-}

-- | To be deprecated in favor of `setDim'`.
setIndex' :: Index ix => ix -> Dim -> Int -> ix
setIndex' ix dim i =
  case setIndex ix dim i of
    Just ix' -> ix'
    Nothing  -> errorDim "setIndex'" dim
{-# INLINE [1] setIndex' #-}

-- | To be deprecated in favor of `getDim'`.
getIndex' :: Index ix => ix -> Dim -> Int
getIndex' ix dim =
  case getIndex ix dim of
    Just ix' -> ix'
    Nothing  -> errorDim "getIndex'" dim
{-# INLINE [1] getIndex' #-}

dropDim' :: Index ix => ix -> Dim -> Lower ix
dropDim' ix dim =
  case dropDim ix dim of
    Just ixl -> ixl
    Nothing  -> errorDim "dropDim'" dim
{-# INLINE [1] dropDim' #-}

pullOutDim' :: Index ix => ix -> Dim -> (Int, Lower ix)
pullOutDim' ix dim =
  case pullOutDim ix dim of
    Just i_ixl -> i_ixl
    Nothing  -> errorDim "pullOutDim'" dim
{-# INLINE [1] pullOutDim' #-}

insertDim' :: Index ix => Lower ix -> Dim -> Int -> ix
insertDim' ix dim i =
  case insertDim ix dim i of
    Just ix' -> ix'
    Nothing  -> errorDim "insertDim'" dim
{-# INLINE [1] insertDim' #-}

errorDim :: String -> Dim -> a
errorDim funName dim = error $ funName ++ ": Dimension is out of reach: " ++ show dim
{-# NOINLINE errorDim #-}

fromDimension :: KnownNat n => Dimension n -> Dim
fromDimension = fromIntegral . natVal
{-# INLINE [1] fromDimension #-}

-- | Type safe way to set value of index at a particular dimension.
--
-- @since 0.2.4
setDimension :: IsIndexDimension ix n => ix -> Dimension n -> Int -> ix
setDimension ix d = setDim' ix (fromDimension d)
{-# INLINE [1] setDimension #-}

-- | Type safe way to extract value of index at a particular dimension.
--
-- @since 0.2.4
getDimension :: IsIndexDimension ix n => ix -> Dimension n -> Int
getDimension ix d = getDim' ix (fromDimension d)
{-# INLINE [1] getDimension #-}


-- | Type safe way of dropping a particular dimension, thus lowering index
-- dimensionality.
--
-- @since 0.2.4
dropDimension :: IsIndexDimension ix n => ix -> Dimension n -> Lower ix
dropDimension ix d = dropDim' ix (fromDimension d)
{-# INLINE [1] dropDimension #-}

-- | Type safe way of pulling out a particular dimension, thus lowering index
-- dimensionality and returning the value at specified dimension.
--
-- @since 0.2.4
pullOutDimension :: IsIndexDimension ix n => ix -> Dimension n -> (Int, Lower ix)
pullOutDimension ix d = pullOutDim' ix (fromDimension d)
{-# INLINE [1] pullOutDimension #-}

-- | Type safe way of inserting a particular dimension, thus raising index dimensionality.
--
-- @since 0.2.4
insertDimension :: IsIndexDimension ix n => ix -> Dimension n -> Lower ix
insertDimension ix d = dropDim' ix (fromDimension d)
{-# INLINE [1] insertDimension #-}


-- | Iterate over N-dimensional space lenarly from start to end in row-major fashion with an
-- accumulator
iterLinearM :: (Index ix, Monad m)
            => ix -- ^ Size
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
iterLinearM_ :: (Index ix, Monad m) =>
                ix -- ^ Size
             -> Int -- ^ Start
             -> Int -- ^ End
             -> Int -- ^ Increment
             -> (Int -> Int -> Bool) -- ^ Continuation condition
             -> (Int -> ix -> m ()) -- ^ Monadic action that takes index in both forms
             -> m ()
iterLinearM_ !sz !k0 !k1 !inc cond f =
  loopM_ k0 (`cond` k1) (+ inc) $ \ !i -> f i (fromLinearIndex sz i)
{-# INLINE iterLinearM_ #-}
