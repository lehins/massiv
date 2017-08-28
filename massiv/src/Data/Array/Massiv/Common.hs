{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
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
  , Construct(..)
  , Source(..)
  , Load(..)
  -- * Computation
  , Comp(..)
  --, Par) -- unsupprted export in GHC 7.10
  , pattern Par
  , PrettyShow(..)
  , module Data.Array.Massiv.Common.Ix
  , module Data.Array.Massiv.Common.Index
  , evaluateAt
  , null
  , length
  , errorIx
  , errorImpossible
  ) where

import           Prelude                         as P hiding (length, null)
import           Control.DeepSeq                (NFData (..), deepseq)
import           Control.Monad.ST               (ST)
import           Data.Array.Massiv.Common.Ix
import           Data.Array.Massiv.Common.Index
import           Data.Proxy
import           Data.Typeable                  (Typeable, showsTypeRep,
                                                 typeRep)
import           Text.Printf

data family Array r ix e :: *

-- | Computation type to use.
data Comp = Seq -- ^ Sequential computation
          | ParOn [Int] -- ^ Parallel computation with a list of capabilities to
                        -- run computation on. Use `Par` to run on all cores.
          deriving (Show, Eq)


instance Monoid Comp where
  mempty = Seq
  mappend = joinComp

joinComp :: Comp -> Comp -> Comp
joinComp Par         _           = Par
joinComp _           Par         = Par
joinComp (ParOn w1)  (ParOn w2)  = ParOn $ w1 ++ w2
joinComp c@(ParOn _) _           = c
joinComp _           c@(ParOn _) = c
joinComp _           _           = Seq


-- | Parallel computation using all available cores.
pattern Par :: Comp
pattern Par = ParOn []


instance NFData Comp where
  rnf comp =
    case comp of
      Seq        -> ()
      Par        -> ()
      ParOn wIds -> wIds `deepseq` ()
  {-# INLINE rnf #-}


-- | Index and size polymorphic arrays.
class (Typeable r, Index ix) => Construct r ix e where

  size :: Array r ix e -> ix

  getComp :: Array r ix e -> Comp

  setComp :: Comp -> Array r ix e -> Array r ix e

  unsafeMakeArray :: Comp -> ix -> (ix -> e) -> Array r ix e




instance (Typeable e, Construct r ix e) => Show (Array r ix e) where
  show arr =
    "<Array " ++
    showsTypeRep (typeRep (Proxy :: Proxy r)) " " ++
    (show (size arr)) ++ " (" ++ showsTypeRep (typeRep (Proxy :: Proxy e)) ")>"


class Construct r ix e => Source r ix e where

  unsafeIndex :: Array r ix e -> ix -> e
  unsafeIndex !arr = unsafeLinearIndex arr . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}



class Construct r ix e => Load r ix e where
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


length :: Construct r ix e => Array r ix e -> Int
length = totalElem . size
{-# INLINE length #-}

null :: Construct r ix e => Array r ix e -> Bool
null !arr = 0 == length arr
{-# INLINE null #-}


errorImpossible :: String -> a
errorImpossible fName =
  error $ fName ++ ": Impossible happened. Please report this error."
--{-# NOINLINE errorImpossible #-}

errorIx :: (Show ix, Show ix') => String -> ix -> ix' -> a
errorIx fName sz ix =
  error $
  fName ++
  ": Index out of bounds: " ++ show ix ++ " for Array of size: " ++ show sz
--{-# NOINLINE errorIx #-}


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
