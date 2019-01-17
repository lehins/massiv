{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Vector
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Vector
  ( fromVector
  , castFromVector
  , toVector
  , castToVector
  , ARepr
  , VRepr
  ) where

import           Control.Monad                          (guard, join, msum)
import           Data.Massiv.Array.Manifest.Boxed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.Primitive
import           Data.Massiv.Array.Manifest.Storable
import           Data.Massiv.Array.Manifest.Unboxed
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Core.Common
import           Data.Typeable
import qualified Data.Vector                            as VB
import qualified Data.Vector.Generic                    as VG
import qualified Data.Vector.Primitive                  as VP
import qualified Data.Vector.Storable                   as VS
import qualified Data.Vector.Unboxed                    as VU

-- | Match vector type to array representation
type family ARepr (v :: * -> *) :: *
-- | Match array representation to a vector type
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


-- | /O(1)/ - conversion from vector to an array with a corresponding
-- representation. Will return `Nothing` if there is a size mismatch, vector has
-- been sliced before or if some non-standard vector type is supplied.
castFromVector :: forall v r ix e. (VG.Vector v e, Typeable v, Mutable r ix e, ARepr v ~ r)
               => Comp
               -> Sz ix -- ^ Size of the result Array
               -> v e -- ^ Source Vector
               -> Maybe (Array r ix e)
castFromVector comp sz vector = do
  guard (totalElem sz == VG.length vector)
  msum
    [ do Refl <- eqT :: Maybe (v :~: VU.Vector)
         uVector <- join $ gcast1 (Just vector)
         return $ UArray {uComp = comp, uSize = sz, uData = uVector}
    , do Refl <- eqT :: Maybe (v :~: VS.Vector)
         sVector <- join $ gcast1 (Just vector)
         return $ SArray {sComp = comp, sSize = sz, sData = sVector}
    , do Refl <- eqT :: Maybe (v :~: VP.Vector)
         VP.Vector 0 _ arr <- join $ gcast1 (Just vector)
         return $ PArray {pComp = comp, pSize = sz, pData = arr}
    , do Refl <- eqT :: Maybe (v :~: VB.Vector)
         bVector <- join $ gcast1 (Just vector)
         arr <- castVectorToArray bVector
         let barr = BArray {bComp = comp, bSize = sz, bData = arr}
         barr `seqArray` return barr
    ]
{-# NOINLINE castFromVector #-}


-- | In case when resulting array representation matches the one of vector's it
-- will do a /O(1)/ - conversion using `castFromVector`, otherwise Vector elements
-- will be copied into a new array. Will throw an error if length of resulting
-- array doesn't match the source vector length.
fromVector ::
     (Typeable v, VG.Vector v a, Mutable (ARepr v) ix a, Mutable r ix a)
  => Comp
  -> Sz ix -- ^ Resulting size of the array
  -> v a -- ^ Source Vector
  -> Array r ix a
fromVector comp sz v =
  case castFromVector comp sz v of
    Just arr -> convert arr
    Nothing ->
      if (totalElem sz /= VG.length v)
        then error $
             "Data.Array.Massiv.Manifest.fromVector: Supplied size: " ++
             show sz ++ " doesn't match vector length: " ++ show (VG.length v)
        else makeArray comp sz ((VG.unsafeIndex v) . toLinearIndex sz)
{-# NOINLINE fromVector #-}


-- | /O(1)/ - conversion from `Mutable` array to a corresponding vector. Will
-- return `Nothing` only if source array representation was not one of `B`, `N`,
-- `P`, `S` or `U`.
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
         return $ castArrayToVector $ bData bArr
    , do Refl <- eqT :: Maybe (r :~: N)
         bArr <- gcastArr arr
         return $ castArrayToVector $ bData $ bArray bArr
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
-- >>> import qualified Data.Vector.Storable as VS
-- >>> toVector (makeArrayR S Par (5 :. 6) (\(i :. j) -> i + j)) :: VS.Vector Int
-- [0,1,2,3,4,5,1,2,3,4,5,6,2,3,4,5,6,7,3,4,5,6,7,8,4,5,6,7,8,9]
--
-- While in this example `S`torable Array will first be converted into `U`nboxed
-- representation in `Par`allel and only after that will be coverted into Unboxed
-- `VU.Vector` in constant time.
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> toVector (makeArrayR S Par (5 :. 6) (\(i :. j) -> i + j)) :: VU.Vector Int
-- [0,1,2,3,4,5,1,2,3,4,5,6,2,3,4,5,6,7,3,4,5,6,7,8,4,5,6,7,8,9]
--
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
    Just v  -> v
    Nothing -> VG.generate (totalElem (size arr)) (unsafeLinearIndex arr)
{-# NOINLINE toVector #-}

