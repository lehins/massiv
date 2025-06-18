{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Massiv.Array.Manifest.Vector
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Manifest.Vector (
  fromVectorM,
  fromVector',
  castFromVector,
  toVector,
  castToVector,
  ARepr,
  VRepr,
) where

import Control.Monad (guard, join, msum)
import Data.Kind
import Data.Massiv.Array.Manifest.Boxed
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.Primitive
import Data.Massiv.Array.Manifest.Storable
import Data.Massiv.Array.Manifest.Unboxed
import Data.Massiv.Core.Common
import Data.Maybe (fromMaybe)
import Data.Typeable
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

-- | Match vector type to array representation
type family ARepr (v :: Type -> Type) :: Type where
  ARepr VU.Vector = U
  ARepr VS.Vector = S
  ARepr VP.Vector = P
  ARepr VB.Vector = BL

-- | Match array representation to a vector type
type family VRepr r :: Type -> Type where
  VRepr U = VU.Vector
  VRepr S = VS.Vector
  VRepr P = VP.Vector
  VRepr B = VB.Vector
  VRepr BN = VB.Vector
  VRepr BL = VB.Vector

-- | /O(1)/ - conversion from vector to an array with a corresponding representation. Will
-- return `Nothing` if there is a size mismatch or if some non-standard vector type is
-- supplied. Is suppplied is the boxed `Data.Vector.Vector` then it's all elements will be
-- evaluated toWHNF, therefore complexity will be /O(n)/
castFromVector
  :: forall v r ix e
   . (VG.Vector v e, Typeable v, Index ix, ARepr v ~ r)
  => Comp
  -> Sz ix
  -- ^ Size of the result Array
  -> v e
  -- ^ Source Vector
  -> Maybe (Array r ix e)
castFromVector comp sz vector = do
  guard (totalElem sz == VG.length vector)
  msum
    [ do
        Refl <- eqT :: Maybe (v :~: VU.Vector)
        uVector <- join $ gcast1 (Just vector)
        return $ UArray{uComp = comp, uSize = sz, uData = uVector}
    , do
        Refl <- eqT :: Maybe (v :~: VS.Vector)
        sVector <- join $ gcast1 (Just vector)
        return $ unsafeResize sz $ fromStorableVector comp sVector
    , do
        Refl <- eqT :: Maybe (v :~: VP.Vector)
        VP.Vector o _ ba <- join $ gcast1 (Just vector)
        return $ PArray{pComp = comp, pSize = sz, pOffset = o, pData = ba}
    , do
        Refl <- eqT :: Maybe (v :~: VB.Vector)
        bVector <- join $ gcast1 (Just vector)
        pure $ unsafeResize sz $ setComp comp $ fromBoxedVector bVector
    ]
{-# NOINLINE castFromVector #-}

-- | In case when resulting array representation matches the one of vector's it
-- will do a /O(1)/ - conversion using `castFromVector`, otherwise Vector elements
-- will be copied into a new array. Will throw an error if length of resulting
-- array doesn't match the source vector length.
--
-- @since 0.3.0
fromVectorM
  :: (MonadThrow m, Typeable v, VG.Vector v a, Manifest r a, Load (ARepr v) ix a, Load r ix a)
  => Comp
  -> Sz ix
  -- ^ Resulting size of the array
  -> v a
  -- ^ Source Vector
  -> m (Array r ix a)
fromVectorM comp sz v =
  case castFromVector comp sz v of
    Just arr -> pure $ convert arr
    Nothing -> do
      guardNumberOfElements sz (Sz (VG.length v))
      pure (makeArrayLinear comp sz (VG.unsafeIndex v))
{-# NOINLINE fromVectorM #-}

-- | Just like `fromVectorM`, but will throw an exception on a mismatched size.
--
-- @since 0.3.0
fromVector'
  :: (HasCallStack, Typeable v, VG.Vector v a, Load (ARepr v) ix a, Load r ix a, Manifest r a)
  => Comp
  -> Sz ix
  -- ^ Resulting size of the array
  -> v a
  -- ^ Source Vector
  -> Array r ix a
fromVector' comp sz = throwEither . fromVectorM comp sz
{-# INLINE fromVector' #-}

-- | /O(1)/ - conversion from `Mutable` array to a corresponding vector. Will
-- return `Nothing` only if source array representation was not one of `B`, `N`,
-- `P`, `S` or `U`.
castToVector
  :: forall v r ix e
   . (Manifest r e, Index ix, VRepr r ~ v)
  => Array r ix e
  -> Maybe (v e)
castToVector arr =
  msum
    [ do
        Refl <- eqT :: Maybe (r :~: U)
        uArr <- gcastArr arr
        return $ uData uArr
    , do
        Refl <- eqT :: Maybe (r :~: S)
        sArr <- gcastArr arr
        return $ toStorableVector sArr
    , do
        Refl <- eqT :: Maybe (r :~: P)
        pArr <- gcastArr arr
        return $ VP.Vector (pOffset pArr) (totalElem (size arr)) $ pData pArr
    , do
        Refl <- eqT :: Maybe (r :~: B)
        bArr <- gcastArr arr
        return $ toBoxedVector $ toLazyArray bArr
    , do
        Refl <- eqT :: Maybe (r :~: BN)
        bArr <- gcastArr arr
        return $ toBoxedVector $ toLazyArray $ unwrapNormalForm bArr
    , do
        Refl <- eqT :: Maybe (r :~: BL)
        bArr <- gcastArr arr
        return $ toBoxedVector bArr
    ]
{-# NOINLINE castToVector #-}

-- | Convert an array into a vector. Will perform a cast if resulting vector is
-- of compatible representation, otherwise memory copy will occur.
--
-- ==== __Examples__
--
-- In this example a `S`torable Array is created and then casted into a Storable
-- `VS.Vector` in costant time:
--
-- >>> import Data.Massiv.Array as A
-- >>> import Data.Massiv.Array.Manifest.Vector (toVector)
-- >>> import qualified Data.Vector.Storable as VS
-- >>> toVector (makeArrayR S Par (Sz2 5 6) (\(i :. j) -> i + j)) :: VS.Vector Int
-- [0,1,2,3,4,5,1,2,3,4,5,6,2,3,4,5,6,7,3,4,5,6,7,8,4,5,6,7,8,9]
--
-- While in this example `S`torable Array will first be converted into `U`nboxed
-- representation in `Par`allel and only after that will be coverted into Unboxed
-- `VU.Vector` in constant time.
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> toVector (makeArrayR S Par (Sz2 5 6) (\(i :. j) -> i + j)) :: VU.Vector Int
-- [0,1,2,3,4,5,1,2,3,4,5,6,2,3,4,5,6,7,3,4,5,6,7,8,4,5,6,7,8,9]
toVector
  :: forall r ix e v
   . ( Manifest r e
     , Load r ix e
     , Manifest (ARepr v) e
     , VG.Vector v e
     , VRepr (ARepr v) ~ v
     )
  => Array r ix e
  -> v e
toVector arr =
  fromMaybe
    (VG.generate (totalElem (size arr)) (unsafeLinearIndex arr))
    (castToVector (convert arr :: Array (ARepr v) ix e))
{-# NOINLINE toVector #-}
