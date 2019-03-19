{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Control.Massiv.Scheduler.Computation
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Massiv.Scheduler.Computation
  ( Comp(.., Par)
  ) where

import Control.DeepSeq (NFData(..), deepseq)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Word

-- | Computation type to use when scheduling work.
data Comp
  = Seq -- ^ Sequential computation
  | ParOn ![Int]
  -- ^ Use `Par` instead to use your CPU to the fullest. Also don't forget to compile
  -- the program with @-threaded@ flag.
  --
  -- Parallel computation with a list of capabilities to run computation
  -- on. Specifying an empty list (@ParOn []@) or using `Par` will result in
  -- utilization of all available capabilities, which are set at runtime by
  -- @+RTS -Nx@ or at compile time by GHC flag @-with-rtsopts=-Nx@,
  -- where @x@ is the number of capabilities. Ommiting @x@ in above flags
  -- defaults to number available cores.
  | ParN {-# UNPACK #-} !Word16
  -- ^ Specify the number of workers that will be handling all the jobs.
  deriving Eq

-- | Parallel computation using all available cores.
pattern Par :: Comp
pattern Par <- ParOn [] where
        Par =  ParOn []

instance Show Comp where
  show Seq        = "Seq"
  show Par        = "Par"
  show (ParOn ws) = "ParOn " ++ show ws
  show (ParN n)   = "ParN " ++ show n
  showsPrec _ Seq  = ("Seq" ++)
  showsPrec _ Par  = ("Par" ++)
  showsPrec 0 comp = (show comp ++)
  showsPrec _ comp = (("(" ++ show comp ++ ")") ++)

instance NFData Comp where
  rnf comp =
    case comp of
      Seq        -> ()
      ParOn wIds -> wIds `deepseq` ()
      ParN n     -> n `deepseq` ()
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
joinComp x y =
  case x of
    Seq -> y
    Par -> Par
    ParOn xs ->
      case y of
        Seq      -> x
        Par      -> Par
        ParOn ys -> ParOn (xs ++ ys)
        ParN n2  -> ParN (max (fromIntegral (length xs)) n2)
    ParN n1 ->
      case y of
        Seq      -> x
        Par      -> Par
        ParOn ys -> ParN (max n1 (fromIntegral (length ys)))
        ParN n2  -> ParN (max n1 n2)
{-# NOINLINE joinComp #-}
