{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.SIMD.Prim
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.SIMD.Prim4 where

import Control.Monad.Primitive
import Control.Monad.ST (runST)
-- import           Data.Array.Massiv.Common
-- import           Data.Array.Massiv.Manifest           (toVector')
-- import           Data.Array.Massiv.Manifest.Primitive
-- import           Data.Array.Massiv.Mutable            (Target)
import Data.Monoid ((<>))
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
import Data.Proxy
import Data.Vector.Primitive as VP
import GHC.Exts
import GHC.Int
import GHC.Prim
import GHC.TypeLits
import GHC.Word


type family Pack (n :: Nat) e :: *

type instance Pack 4 Word8 = W8X4

type instance Pack 2 Word16 = W16X2

type instance Pack 2 Int16 = I16X2

--type instance Pack 2 Word64 = W64X2

data W8X4 = W8X4# {-# UNPACK #-} !Word#

data W16X2 = W16X2# {-# UNPACK #-} !Word#

data I16X2 = I16X2# {-# UNPACK #-} !Int#

--data W64X2 = W64X2# Word64X2#

class Packed n e where

  pack2 :: Unpack n e -> Pack n e

  unpack2 :: Pack n e -> (e, e)


instance Pack 2 Word16 where
  data P 2 Word16 = W16X2#
                    {-# UNPACK #-} !Word#
  data U 2 Word16 = W16X2
                    {-# UNPACK #-} !Word16
                    {-# UNPACK #-} !Word16 deriving Show

  pack2 (W16X2 (W16# w0#) (W16# w1#)) = W16X2# (or# w0# (byteSwap16# w1#))

  unpack2 (W16X2# w#) = W16X2 (W16# (and# 0xFF## w#)) (W16# (and# 0x00FF## (byteSwap16# w#)))
