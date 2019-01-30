{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators   #-}
-- |
-- Module      : Data.Massiv.Core.Index
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
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
  , splitWith_
  , module Data.Massiv.Core.Index.Tuple
  -- * Error functions
  , errorIx
  -- , errorSizeMismatch
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
zeroIndex :: Index ix => ix
zeroIndex = pureIndex 0
{-# INLINE [1] zeroIndex #-}

-- | Checks whether array with this size can hold at least one element.
isNonEmpty :: Index ix => Sz ix -> Bool
isNonEmpty !sz = isSafeIndex sz zeroIndex
{-# INLINE [1] isNonEmpty #-}
-- TODO: benchmark against (also adjust `isEmpty` with fastest):
-- - foldlIndex (*) 1 (unSz sz) /= 0
-- - foldlIndex ((&&) . (==0)) True (unSz sz)
-- - totalElem sz == 0


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
setDim' ix dim = either throw id . setDimM ix dim
{-# INLINE [1] setDim' #-}

setDim :: Index ix => ix -> Dim -> Int -> Maybe ix
setDim = setDimM
{-# INLINE [1] setDim #-}
{-# DEPRECATED setDim "In favor of more general `setDimM`" #-}

getDim' :: Index ix => ix -> Dim -> Int
getDim' ix = either throw id . getDimM ix
{-# INLINE [1] getDim' #-}

getDim :: Index ix => ix -> Dim -> Maybe Int
getDim = getDimM
{-# INLINE [1] getDim #-}
{-# DEPRECATED getDim "In favor of more general `getDimM`" #-}

-- | Remove a dimension from the index.
--
-- @since 0.3.0
dropDimM :: (MonadThrow m, Index ix) => ix -> Dim -> m (Lower ix)
dropDimM ix = fmap snd . pullOutDimM ix
{-# INLINE [1] dropDimM #-}

-- | Remove a dimension from the index.
--
-- @since 0.1.0
dropDim :: Index ix => ix -> Dim -> Maybe (Lower ix)
dropDim = dropDimM
{-# INLINE [1] dropDim #-}
{-# DEPRECATED dropDim "In favor of more general `dropDimM`" #-}

dropDim' :: Index ix => ix -> Dim -> Lower ix
dropDim' ix = either throw id . dropDimM ix
{-# INLINE [1] dropDim' #-}

pullOutDim' :: Index ix => ix -> Dim -> (Int, Lower ix)
pullOutDim' ix = either throw id . pullOutDimM ix
{-# INLINE [1] pullOutDim' #-}

pullOutDim :: Index ix => ix -> Dim -> Maybe (Int, Lower ix)
pullOutDim = pullOutDimM
{-# INLINE [1] pullOutDim #-}
{-# DEPRECATED pullOutDim "In favor of more general `pullOutDimM`" #-}

insertDim' :: Index ix => Lower ix -> Dim -> Int -> ix
insertDim' ix dim = either throw id . insertDimM ix dim
{-# INLINE [1] insertDim' #-}

insertDim :: Index ix => Lower ix -> Dim -> Int -> Maybe ix
insertDim = insertDimM
{-# INLINE [1] insertDim #-}
{-# DEPRECATED insertDim "In favor of more general `insertDimM`" #-}

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
-- @since 0.2.5
insertDimension :: IsIndexDimension ix n => Lower ix -> Dimension n -> Int -> ix
insertDimension ix d = insertDim' ix (fromDimension d)
{-# INLINE [1] insertDimension #-}

-- | Iterator for the index. Same as `iterM`, but pure.
iter :: Index ix => ix -> ix -> ix -> (Int -> Int -> Bool) -> a -> (ix -> a -> a) -> a
iter sIx eIx incIx cond acc f =
  runIdentity $ iterM sIx eIx incIx cond acc (\ix -> return . f ix)
{-# INLINE iter #-}


-- | Iterate over N-dimensional space lenarly from start to end in row-major fashion with an
-- accumulator
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



splitWith_ :: (Index ix, Monad m) =>
  Int -> (m () -> m a) -> Sz ix -> (ix -> b) -> (ix -> b -> m ()) -> m a
splitWith_ numChunks with sz index write =
  let totalLength = totalElem sz
  in splitLinearly numChunks totalLength $ \chunkLength slackStart -> do
       loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
         with $ iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ _i !ix -> write ix (index ix)
       with $ iterLinearM_ sz slackStart totalLength 1 (<) $ \ _i !ix -> write ix (index ix)
{-# INLINE splitWith_ #-}



-- | Helper function for throwing out of bounds errors
errorIx :: (Show ix, Show ix') => String -> ix -> ix' -> a
errorIx fName sz ix =
  error $
  fName ++
  ": Index out of bounds: (" ++ show ix ++ ") for Array of size: (" ++ show sz ++ ")"
{-# NOINLINE errorIx #-}


-- -- | Helper function for throwing error when sizes do not match
-- errorSizeMismatch :: (Show ix, Show ix') => String -> ix -> ix' -> a
-- errorSizeMismatch fName sz sz' =
--   error $ fName ++ ": Mismatch in size of arrays " ++ show sz ++ " vs " ++ show sz'
-- {-# NOINLINE errorSizeMismatch #-}

-- splitWith_ :: (Index ix, Monad m) =>
--   Int -> (m () -> m a) -> ix -> (ix -> b) -> (ix -> b -> m ()) -> m a
-- splitWith_ numChunks with sz index write =
--   let totalLength = totalElem sz
--   in splitLinearly numChunks totalLength $ \chunkLength slackStart -> do
--        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--          with $ loopM_ start (< (start + chunkLength)) (+ 1) $ \ !i ->
--            let ix = fromLinearIndex sz i in write ix (index ix)
--        with $ loopM_ slackStart (< totalLength) (+ 1) $ \ !i ->
--            let ix = fromLinearIndex sz i in write ix (index ix)
-- {-# INLINE splitWith_ #-}
