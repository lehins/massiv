{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
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
  , Border(..)
  , handleBorderIndex
  , module Data.Massiv.Core.Index.Class
  , isSafeSize
  , isNonEmpty
  , headDim
  , tailDim
  , lastDim
  , initDim
  , iterLinearM
  , iterLinearM_
  , module Data.Massiv.Core.Iterator
  ) where

import           Control.DeepSeq
import           Data.Massiv.Core.Index.Class
import           Data.Massiv.Core.Index.Ix
import           Data.Massiv.Core.Iterator


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


-- | Iterate over N-dimensional space from start to end with accumulator
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

