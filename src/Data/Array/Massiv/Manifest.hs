{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
  -- * Boxed
  , B(..)
  -- * Primitive
  , P(..)
  -- * Storable
  , S(..)
  -- * Unboxed
  , U(..)
  -- * Vector Conversion
  , toVector
  , toVector'
  , fromVector
  , fromVector'
  , ARepr
  , VRepr
  -- * Indexing
  , (!)
  , index
  , (!?)
  , maybeIndex
  , (?)
  , defaultIndex
  , borderIndex
  ) where

import           Control.Monad                        (guard, join, msum)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest.Boxed
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Manifest.Primitive
import           Data.Array.Massiv.Manifest.Storable
import           Data.Array.Massiv.Manifest.Unboxed
import           Data.Array.Massiv.Mutable
import           Data.Maybe                           (fromMaybe)
import           Data.Typeable
import qualified Data.Vector                          as VB
import qualified Data.Vector.Generic                  as VG
import qualified Data.Vector.Primitive                as VP
import qualified Data.Vector.Storable                 as VS
import qualified Data.Vector.Unboxed                  as VU


type family ARepr (v :: * -> *) :: *
type family VRepr r :: * -> *

type instance ARepr VU.Vector = U
type instance ARepr VS.Vector = S
type instance ARepr VP.Vector = P
type instance ARepr VB.Vector = B
type instance VRepr U = VU.Vector
type instance VRepr S = VS.Vector
type instance VRepr P = VP.Vector
type instance VRepr B = VB.Vector


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
index arr ix = borderIndex (Fill (errorIx "index" (size arr) ix)) arr ix
{-# INLINE index #-}

-- | /O(1)/ conversion from vector to an array with a corresponding
-- representation. Will return `Nothing` if there is a size mismatch or some
-- uncommon vector type is supplied.
fromVector :: forall v r ix e. (VG.Vector v e, Typeable v, Target r ix e, ARepr v ~ r)
           => ix -- ^ Size of the result Array
           -> v e -- ^ Source Vector
           -> Maybe (Array r ix e)
fromVector sz vector = do
  guard (totalElem sz == VG.length vector)
  msum
    [ do Refl <- eqT :: Maybe (v :~: VU.Vector)
         uVector <- join $ gcast1 (Just vector)
         return $ UArray {uComp = Seq, uSize = sz, uData = uVector}
    , do Refl <- eqT :: Maybe (v :~: VS.Vector)
         sVector <- join $ gcast1 (Just vector)
         return $ SArray {sComp = Seq, sSize = sz, sData = sVector}
    , do Refl <- eqT :: Maybe (v :~: VP.Vector)
         pVector <- join $ gcast1 (Just vector)
         return $ PArray {pComp = Seq, pSize = sz, pData = pVector}
    , do Refl <- eqT :: Maybe (v :~: VB.Vector)
         bVector <- join $ gcast1 (Just vector)
         return $ BArray {bComp = Seq, bSize = sz, bData = bVector}
    ]
{-# INLINE fromVector #-}


-- | Just like `fromVector`, but will throw an appropriate error message
fromVector' :: (VG.Vector v e, Typeable v, Target r ix e, ARepr v ~ r)
            => ix -> v e -> Array r ix e
fromVector' sz vector =
  case fromVector sz vector of
    Just arr -> arr
    Nothing ->
      error $
      "fromVector': " ++
      if (totalElem sz == VG.length vector)
        then "Supplied size: " ++
             show sz ++
             " doesn't match vector length: " ++ show (VG.length vector)
        else "Unsupported Vector type"
{-# INLINE fromVector' #-}


-- | /O(1)/ conversion from `Target` array to a corresponding vector. Will
-- return `Nothing` only if source array representation was not one of `B`,
-- `P`, `S` or `U`
toVector :: forall v r ix e . (VG.Vector v e, Target r ix e, VRepr r ~ v)
         => Array r ix e -> Maybe (v e)
toVector arr =
  msum
    [ do Refl <- eqT :: Maybe (r :~: U)
         uArr <- gcastArr arr
         return $ uData uArr
    , do Refl <- eqT :: Maybe (r :~: S)
         sArr <- gcastArr arr
         return $ sData sArr
    , do Refl <- eqT :: Maybe (r :~: P)
         pArr <- gcastArr arr
         return $ pData pArr
    , do Refl <- eqT :: Maybe (r :~: B)
         bArr <- gcastArr arr
         return $ bData bArr
    ]
{-# INLINE toVector #-}


-- | Just like `toVector`, but will throw an appropriate error message
toVector' :: (VG.Vector v e, Target r ix e, VRepr r ~ v)
          => Array r ix e -> v e
toVector' =
  fromMaybe (error "toVector': Unsupported Array representation type") . toVector
{-# INLINE toVector' #-}
