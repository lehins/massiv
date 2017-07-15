{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.SIMD.Base
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.SIMD.Base where

import Data.Array.Massiv.Common


data instance Array VF ix e = VFArray { vfComp  :: Comp
                                      , vfSize  :: !ix
                                      , vfWrite :: Ptr e -> Ptr e -> IO () }


--instance Massiv



-- type instance SIMDMap Int32 = Int32X4# -> Int32X4#

-- type family SIMDFunc2 a :: *

-- type instance SIMDFunc2 Int32 = Int32X4# -> Int32X4# -> Int32X4#

type family SIMDMap r e :: *

type family SIMDZipWith r e :: *


class SIMD r ix e where

  mapSIMD :: SIMDMap r e -> Array r ix e -> Array r ix e


type SIMDMap VF e = Ptr e -> Ptr e -> IO ()

type SIMDZipWith VF e = Ptr e -> Ptr e -> Ptr e -> IO ()


