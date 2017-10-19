{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Core.Computation
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Computation
  ( Comp(..)
  , pattern Par
  ) where

import           Control.DeepSeq (NFData (..), deepseq)

-- | Computation type to use.
data Comp = Seq -- ^ Sequential computation
          | ParOn [Int] -- ^ Use `Par` to run on all cores. Parallel computation
                        -- with a list of capabilities to run computation on.
          deriving (Show, Eq)

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

instance Monoid Comp where
  mempty = Seq
  {-# INLINE mempty #-}
  mappend = joinComp
  {-# INLINE mappend #-}


joinComp :: Comp -> Comp -> Comp
joinComp Par         _           = Par
joinComp _           Par         = Par
joinComp (ParOn w1)  (ParOn w2)  = ParOn $ w1 ++ w2
joinComp c@(ParOn _) _           = c
joinComp _           c@(ParOn _) = c
joinComp _           _           = Seq
{-# INLINE joinComp #-}
