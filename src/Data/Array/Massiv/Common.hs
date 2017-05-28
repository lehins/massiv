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
  , makeArray
  , Massiv(..)
  , Source(..)
  , Load(..)
  , Comp(..)
  , PrettyShow(..)
  , module Data.Array.Massiv.Common.Index
  , evaluateAt
  , errorIx
  , errorImpossible
  ) where

import           Control.DeepSeq                (NFData (..), deepseq)
import           Control.Monad.ST               (ST)
import           Data.Array.Massiv.Common.Index hiding (Z)
import           Data.Proxy
import           Data.Typeable                  (Typeable, showsTypeRep,
                                                 typeRep)
import           Text.Printf

data family Array r ix e :: *


data Comp = Seq | Par | ParOn [Int] deriving (Show, Eq)


instance NFData Comp where
  rnf comp =
    case comp of
      Seq        -> ()
      Par        -> ()
      ParOn wIds -> wIds `deepseq` ()
  {-# INLINE rnf #-}


-- | Index and size polymorphic arrays.
class Index ix => Massiv r ix e where

  size :: Array r ix e -> ix

  getComp :: Array r ix e -> Comp

  setComp :: Array r ix e -> Comp -> Array r ix e

  unsafeMakeArray :: Index ix => Comp -> ix -> (ix -> e) -> Array r ix e


makeArray :: Massiv r ix e => Comp -> ix -> (ix -> e) -> Array r ix e
makeArray !c = unsafeMakeArray c . liftIndex (max 0)
{-# INLINE makeArray #-}



instance (Typeable r, Typeable e, Massiv r ix e) => Show (Array r ix e) where
  show arr =
    "<Array " ++
    showsTypeRep (typeRep (Proxy :: Proxy r)) "(" ++ show (size arr) ++ ") " ++
    showsTypeRep (typeRep (Proxy :: Proxy e)) ">"


class Massiv r ix e => Source r ix e where

  unsafeIndex :: Array r ix e -> ix -> e
  unsafeIndex !arr = unsafeLinearIndex arr . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}



class Massiv r ix e => Load r ix e where
  -- | Load an array into memory sequentially
  loadS
    :: Array r ix e -- ^ Array that is being loaded
    -> (Int -> ST s e) -- ^ Function that reads an element from target array
    -> (Int -> e -> ST s ()) -- ^ Function that writes an element into target array
    -> ST s ()

  -- | Load an array into memory in parallel
  loadP
    :: [Int] -- ^ List of capabilities to run workers on, as described in
             -- `Control.Concurrent.forkOn`. Empty list will imply all
             -- capabilities, i.e. run on all cores available through @+RTS -N@.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> IO e) -- ^ Function that reads an element from target array
    -> (Int -> e -> IO ()) -- ^ Function that writes an element into target array
    -> IO ()



errorImpossible :: String -> a
errorImpossible fName =
  error $ fName ++ ": Impossible happened. Please report this error."

errorIx :: (Index ix, Index ix') => String -> ix -> ix' -> a
errorIx fName sz ix =
  error $
  fName ++
  ": Index out of bounds: " ++ show ix ++ " for Array of size: " ++ show sz


-- | This is just like safe `Data.Array.Massiv.Manifest.index` function, but it
-- allows getting values from delayed arrays as well as manifest. As the name
-- suggests indexing into delayed array with the same index multiple times will
-- cause evaluation of the value each time.
evaluateAt :: Source r ix e => Array r ix e -> ix -> e
evaluateAt !arr !ix =
  handleBorderIndex
    (Fill (errorIx "evaluateAt" (size arr) ix))
    (size arr)
    (unsafeIndex arr)
    ix
{-# INLINE evaluateAt #-}


class PrettyShow ix where

  prettyShow :: PrintfArg e => String -> Array r ix e -> String
