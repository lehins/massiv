{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-- |
-- Module      : Data.Array.Massiv.Manifest
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest
  ( -- * Manifest
    Manifest
  , toManifest
  , M
  , computeS
  , computeP
  , computeOnP
  , computeAsS
  , computeAsP
  -- * Boxed
  , B.B(..)
  , B.computeBoxedS
  , B.computeBoxedP
  -- * Primitive
  , P.P(..)
  , P.computePrimitiveS
  , P.computePrimitiveP
  -- * Storable
  , S.S(..)
  , S.computeStorableS
  , S.computeStorableP
  -- * Unboxed
  , U.U(..)
  , U.computeUnboxedS
  , U.computeUnboxedP
  -- * Indexing
  , (!)
  , index
  , (!?)
  , maybeIndex
  , (?)
  , defaultIndex
  , borderIndex
  ) where

import           Data.Array.Massiv.Common
import qualified Data.Array.Massiv.Manifest.Boxed     as B
import           Data.Array.Massiv.Manifest.Internal
import qualified Data.Array.Massiv.Manifest.Primitive as P
import qualified Data.Array.Massiv.Manifest.Storable  as S
import qualified Data.Array.Massiv.Manifest.Unboxed   as U
import           Data.Array.Massiv.Mutable            (Target, loadTargetOnP,
                                                       loadTargetS)
import           System.IO.Unsafe                     (unsafePerformIO)


computeS
  :: (Load r' ix e, Target r ix e)
  => Array r' ix e -> Array r ix e
computeS = loadTargetS
{-# INLINE computeS #-}


computeP
  :: (Load r' ix e, Target r ix e)
  => Array r' ix e -> Array r ix e
computeP = unsafePerformIO . loadTargetOnP []
{-# INLINE computeP #-}


computeOnP
  :: (Load r' ix e, Target r ix e)
  => [Int] -> Array r' ix e -> Array r ix e
computeOnP wIds = unsafePerformIO . loadTargetOnP wIds
{-# INLINE computeOnP #-}


computeAsS
  :: (Load r' ix e, Target r ix e)
  => r -> Array r' ix e -> Array r ix e
computeAsS _ = computeS
{-# INLINE computeAsS #-}

computeAsP
  :: (Load r' ix e, Target r ix e)
  => r -> Array r' ix e -> Array r ix e
computeAsP _ = computeP
{-# INLINE computeAsP #-}


-- | Infix version of `index`.
(!) :: Manifest r ix e => Array r ix e -> ix -> e
(!) = index
{-# INLINE (!) #-}


-- | Infix version of `maybeIndex`.
(!?) :: Manifest r ix e => Array r ix e -> ix -> Maybe e
(!?) = maybeIndex
{-# INLINE (!?) #-}


(?) :: Manifest r ix e => Maybe (Array r ix e) -> ix -> Maybe e
(?) Nothing    = const Nothing
(?) (Just arr) = (arr !?)
{-# INLINE (?) #-}


maybeIndex :: Manifest r ix e => Array r ix e -> ix -> Maybe e
maybeIndex arr = handleBorderIndex (Fill Nothing) (size arr) (Just . unsafeIndex arr)
{-# INLINE maybeIndex #-}


defaultIndex :: Manifest r ix e => e -> Array r ix e -> ix -> e
defaultIndex defVal = borderIndex (Fill defVal)
{-# INLINE defaultIndex #-}


borderIndex :: Manifest r ix e => Border e -> Array r ix e -> ix -> e
borderIndex border arr = handleBorderIndex border (size arr) (unsafeIndex arr)
{-# INLINE borderIndex #-}


index :: Manifest r ix e => Array r ix e -> ix -> e
index arr ix = borderIndex (Fill (errorIx "index" arr ix)) arr ix
{-# INLINE index #-}

