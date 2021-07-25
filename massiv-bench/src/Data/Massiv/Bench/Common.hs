{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Massiv.Bench.Common
  ( makeRandomArray
  , showsType
  , stdGen
  ) where

import Data.Massiv.Array
import Data.Typeable
import System.Random

stdGen :: StdGen
stdGen = mkStdGen 2020

-- | Use Typeable to show the type.
showsType :: forall t . Typeable t => ShowS
showsType = showsTypeRep (typeRep (Proxy :: Proxy t))

makeRandomArray :: (Index ix, Manifest r e, Random e) => Sz ix -> IO (Array r ix e)
makeRandomArray sz = do
  gen <- newStdGen
  pure $! snd $ randomArrayS gen sz random
