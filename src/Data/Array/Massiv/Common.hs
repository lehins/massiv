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
  , PrettyShow(..)
  , module Data.Array.Massiv.Common.Index
  , safeIndex
  , errorIx
  , errorImpossible
  ) where

import           Control.Monad.ST               (ST)
import           Data.Array.Massiv.Common.Index hiding (Z)
import           Data.Proxy
import           Data.Typeable                  (Typeable, showsTypeRep,
                                                 typeRep)
import           Text.Printf

data family Array r ix e :: *


-- | Index and size polymorphic arrays.
class (Typeable r, Index ix) => Massiv r ix e where

  size :: Array r ix e -> ix

  makeArray :: Index ix => ix -> (ix -> e) -> Array r ix e



instance Massiv r ix e => Show (Array r ix e) where
  show arr =
    "<Array " ++
    showsTypeRep (typeRep (Proxy :: Proxy r)) ": " ++ show (size arr) ++ ">"


-- instance (Show e, Source r DIM1 e) => Show (Array r DIM1 e) where
--   show arr = "<Array DIM1 (" ++ show (size arr) ++ ")>:\n" ++ show (foldrA (:) [] arr)

-- instance (Show e, Source r DIM2 e) => Show (Array r DIM2 e) where
--   show arr = "<Array DIM2 " ++ show (size arr) ++ ">:\n" ++ show (foldrA (:) [] arr)


class Massiv r ix e => Source r ix e where

  unsafeIndex :: Array r ix e -> ix -> e
  unsafeIndex !arr !ix = unsafeLinearIndex arr $ toLinearIndex (size arr) ix
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}


class Index ix => Load r ix where
  -- | Load an array into memory sequentially
  loadS
    :: Massiv r ix e =>
       Array r ix e -- ^ Array that is being loaded
    -> (Int -> ST s e) -- ^ Function that reads an element
    -> (Int -> e -> ST s ()) -- ^ Function that writes an element
    -> ST s ()

  -- | Load an array into memory in parallel
  loadP
    :: Massiv r ix e =>
       [Int] -- ^ List of capabilities to run workers on, as described in
             -- `Control.Concurrent.forkOn`. Empty will imply all that are available,
             -- i.e. run on all cores available through @+RTS -N@.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> IO e) -- ^ Function that reads an element
    -> (Int -> e -> IO ()) -- ^ Function that writes an element
    -> IO ()




errorImpossible :: String -> a
errorImpossible fName =
  error $ fName ++ ": Impossible happened. Please report this error."

errorIx :: (Massiv r ix e, Index ix') => String -> Array r ix e -> ix' -> a
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


class PrettyShow ix where

  prettyShow :: PrintfArg e => String -> Array r ix e -> String
