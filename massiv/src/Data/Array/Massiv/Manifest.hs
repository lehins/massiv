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
  , N(..)
  -- * Primitive
  , P(..)
  , Prim
  -- * Storable
  , S(..)
  , Storable
  -- * Unboxed
  , U(..)
  , Unbox
  -- * Vector Conversion
  , toVector
  , castToVector
  , fromVector
  , castFromVector
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
import           Data.Array.Massiv.Manifest.BoxedStrict
import           Data.Array.Massiv.Manifest.BoxedNF
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Manifest.Primitive
import           Data.Array.Massiv.Manifest.Storable
import           Data.Array.Massiv.Manifest.Unboxed
import           Data.Array.Massiv.Mutable
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
type instance VRepr N = VB.Vector

infixr 5 !, !?, ?

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
-- representation. Will return `Nothing` if there is a size mismatch, vector has
-- been sliced before or if some non-standard vector type is supplied.
castFromVector :: forall v r ix e. (VG.Vector v e, Typeable v, Mutable r ix e, ARepr v ~ r)
           => ix -- ^ Size of the result Array
           -> v e -- ^ Source Vector
           -> Maybe (Array r ix e)
castFromVector sz vector = do
  guard (totalElem sz == VG.length vector)
  msum
    [ do Refl <- eqT :: Maybe (v :~: VU.Vector)
         uVector <- join $ gcast1 (Just vector)
         return $ UArray {uComp = Seq, uSize = sz, uData = uVector}
    , do Refl <- eqT :: Maybe (v :~: VS.Vector)
         sVector <- join $ gcast1 (Just vector)
         return $ SArray {sComp = Seq, sSize = sz, sData = sVector}
    , do Refl <- eqT :: Maybe (v :~: VP.Vector)
         VP.Vector 0 _ arr <- join $ gcast1 (Just vector)
         return $ PArray {pComp = Seq, pSize = sz, pData = arr}
    , do Refl <- eqT :: Maybe (v :~: VB.Vector)
         bVector <- join $ gcast1 (Just vector)
         arr <- castVectorToArray bVector
         return $ BArray {bComp = Seq, bSize = sz, bData = arr}
    ]
{-# INLINE castFromVector #-}

-- TODO: fromVector could utilize vector copying, in case if one was previously
-- slice. Currently it will simply do an element-wise copy.
-- | Just like `fromVector`, but will throw an appropriate error message
-- fromVector :: (VG.Vector v e, Typeable v, Mutable r ix e, ARepr v ~ r)
--             => ix -> v e -> Array r ix e
fromVector ::
     (Typeable v, VG.Vector v a, Mutable (ARepr v) ix a, Mutable r ix a)
  => ix -- ^ Resulting size of the array
  -> v a -- ^ Source Vector
  -> Array r ix a
fromVector sz v =
  case castFromVector sz v of
    Just arr -> convert arr
    Nothing ->
      if (totalElem sz /= VG.length v)
        then error $
             "Data.Array.Massiv.Manifest.fromVector: Supplied size: " ++
             show sz ++ " doesn't match vector length: " ++ show (VG.length v)
        else unsafeMakeArray Seq sz ((v VG.!) . toLinearIndex sz)
{-# INLINE fromVector #-}


-- | /O(1)/ conversion from `Mutable` array to a corresponding vector. Will
-- return `Nothing` only if source array representation was not one of `B`, `N`,
-- `P`, `S` or `U`
castToVector :: forall v r ix e . (VG.Vector v e, Mutable r ix e, VRepr r ~ v)
         => Array r ix e -> Maybe (v e)
castToVector arr =
  msum
    [ do Refl <- eqT :: Maybe (r :~: U)
         uArr <- gcastArr arr
         return $ uData uArr
    , do Refl <- eqT :: Maybe (r :~: S)
         sArr <- gcastArr arr
         return $ sData sArr
    , do Refl <- eqT :: Maybe (r :~: P)
         pArr <- gcastArr arr
         return $ VP.Vector 0 (totalElem (size arr)) $ pData pArr
    , do Refl <- eqT :: Maybe (r :~: B)
         bArr <- gcastArr arr
         return $ vectorFromArray (size arr) $ bData bArr
    , do Refl <- eqT :: Maybe (r :~: N)
         bArr <- gcastArr arr
         return $ vectorFromArray (size arr) $ nData bArr
    ]
{-# INLINE castToVector #-}

toVector ::
     forall r ix e v.
     ( Manifest r ix e
     , Mutable (ARepr v) ix e
     , VG.Vector v e
     , VRepr (ARepr v) ~ v
     )
  => Array r ix e
  -> v e
toVector arr =
  case castToVector (convert arr :: Array (ARepr v) ix e) of
    Just v -> v
    Nothing -> VG.generate (totalElem (size arr)) (unsafeLinearIndex arr)
{-# INLINE toVector #-}
