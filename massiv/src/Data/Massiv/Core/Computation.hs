{-# LANGUAGE CPP                 #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Core.Computation
-- Copyright   : (c) Alexey Kuleshevich 2018
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
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.Validity


-- | Computation type to use.
data Comp
  = Seq -- ^ Sequential computation
  | ParOn [Int]
  -- ^ Use `Par` instead to use your CPU to the fullest. Also don't forget to compile
  -- the program with @-threaded@ flag.
  --
  -- Parallel computation with a list of capabilities to run computation
  -- on. Specifying an empty list (@ParOn []@) or using `Par` will result in
  -- utilization of all available capabilities, which are set at runtime by
  -- @+RTS -Nx@ or at compile time by GHC flag @-with-rtsopts=-Nx@,
  -- where @x@ is the number of capabilities. Ommiting @x@ in above flags
  -- defaults to number available cores.
  deriving (Show, Eq)

instance Validity Comp where
    validate Seq = valid
    validate (ParOn x) = delve "ParOn" x

-- | Parallel computation using all available cores.
pattern Par :: Comp
pattern Par <- ParOn [] where
        Par =  ParOn []

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

instance Semigroup Comp where
  (<>) = joinComp
  {-# INLINE (<>) #-}


joinComp :: Comp -> Comp -> Comp
joinComp Par         _           = Par
joinComp _           Par         = Par
joinComp (ParOn w1)  (ParOn w2)  = ParOn $ w1 ++ w2
joinComp c@(ParOn _) _           = c
joinComp _           c@(ParOn _) = c
joinComp _           _           = Seq
{-# INLINE joinComp #-}
