{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Common
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common
  ( Array
  , Massiv(..)
  , Source(..)
  , Load(..)
  , module Data.Array.Massiv.Common.Index
  --, module Data.Array.Massiv.Common.Iterator
  , ifoldl
  , ifoldr
  , safeIndex
  , errorIx
  ) where

import           Control.Monad.ST               (ST)
import           Data.Array.Massiv.Common.Index hiding (Z)
import           Data.Proxy
import           Data.Typeable                  (Typeable, showsTypeRep,
                                                 typeRep)

data family Array r ix e :: *


-- | Immutable, shape polymorphic array construction and indexing.
class (Typeable r, Index ix) => Massiv r ix where

  size :: Array r ix e -> ix



instance Massiv r ix =>
         Show (Array r ix e) where
  show arr =
    "<Array " ++
    showsTypeRep (typeRep (Proxy :: Proxy r)) ": " ++ show (size arr) ++ ">"


-- instance (Show e, Source r DIM1 e) => Show (Array r DIM1 e) where
--   show arr = "<Array DIM1 (" ++ show (size arr) ++ ")>:\n" ++ show (foldrA (:) [] arr)

-- instance (Show e, Source r DIM2 e) => Show (Array r DIM2 e) where
--   show arr = "<Array DIM2 " ++ show (size arr) ++ ">:\n" ++ show (foldrA (:) [] arr)


class Massiv r ix => Source r ix e where

  unsafeIndex :: Massiv r ix => Array r ix e -> ix -> e
  unsafeIndex !arr = unsafeLinearIndex arr . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}


class Massiv r ix => Load r ix where
  -- | Load an array into memory sequentially
  loadS
    :: Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> ST s ()) -- ^ Write element
    -> ST s ()

  -- | Load an array into memory in parallel
  loadP
    :: Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> IO ()) -- ^ Write element
    -> IO ()


ifoldl
  :: Source r ix a
  => (ix -> b -> a -> b) -> b -> Array r ix a -> b
ifoldl f !acc !arr = iter zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f ix a (unsafeIndex arr ix)
{-# INLINE ifoldl #-}

ifoldr
  :: Source r ix a
  => (ix -> a -> b -> b) -> b -> Array r ix a -> b
ifoldr f !acc !arr =
  iter
    (liftIndex (subtract 1) (size arr))
    zeroIndex
    (-1)
    (>=)
    acc $ \ !ix !a -> f ix (unsafeIndex arr ix) a
{-# INLINE ifoldr #-}

-- foldrA
--   :: Source r ix a
--   => (a -> b -> b) -> b -> Array r ix a -> b
-- foldrA f !acc !arr =
--   iter
--     (liftIndex (subtract 1) (size arr))
--     zeroIndex
--     (-1)
--     (>=)
--     acc $ \ !ix !a -> f (unsafeIndex arr ix) a
-- {-# INLINE foldrA #-}



errorIx :: (Massiv r ix, Index ix') => String -> Array r ix e -> ix' -> a
errorIx fName arr ix =
  error $ fName ++ ": Index out of bounds: " ++ show ix ++ " for: " ++ show arr

safeIndex :: Source r ix e => Array r ix e -> ix -> e
safeIndex !arr !ix =
  handleBorderIndex
    (Fill (errorIx "safeIndex" arr ix))
    (size arr)
    (unsafeIndex arr)
    ix
{-# INLINE safeIndex #-}
