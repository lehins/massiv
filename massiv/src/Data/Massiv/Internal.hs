{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Internal
  ( Massiv(..)
  , Layout(..)
  , computeM
  , delayM
  , unwrapMassiv
  ) where
import           Control.DeepSeq
import           Data.Complex               (Complex)
import           Data.Functor.Const         (Const)
import           Data.Functor.Identity      (Identity)
import           Data.Int
import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Core
import           Data.Primitive.Types       (Addr)
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr                (FunPtr, IntPtr, Ptr, WordPtr)
import           Foreign.StablePtr          (StablePtr)
import           GHC.Fingerprint            (Fingerprint)
import           GHC.TypeLits


class Mutable (Repr e) ix e => Layout ix e where
  type Repr e :: *
  type Repr e = B


-- | Array like data structure that can be indexed in constant time, manipulated
-- in all common ways that this data structure type permits with ability do
-- processing in parallel. Prefered memory layout will be automatically selected
-- depending on the type of the elements. Custom data types should use `Layout`
-- class to decide on representation.
data Massiv ix e = Massiv !(Array (Repr e) ix e)

instance NFData (Array (Repr e) ix e) => NFData (Massiv ix e) where
  rnf (Massiv arr) = rnf arr
  {-# INLINE [1] rnf #-}


type family a /> b :: * where
  P /> P = U
  P /> U = U
  U /> P = U
  a /> b = B

-- | Load an `Array` into memory as a `Massiv`
computeM :: (Load r ix e, Layout ix e) => Array r ix e -> Massiv ix e
computeM = Massiv . compute
{-# INLINE [1] computeM #-}


-- | Unwrap `Massiv` as a Delayed array.

-- #if __GLASGOW_HASKELL__ >= 820
delayM :: Layout ix e => Massiv ix e -> Array M ix e
delayM (Massiv arr) = toManifest arr
-- #else
-- delayM :: Layout ix e => Massiv ix e -> Array D ix e
-- delayM (Massiv arr) = delay arr
-- #endif
{-# INLINE [1] delayM #-}

{-# RULES
"delayM/computeM" forall arr . delayM (computeM arr) = arr
 #-}

-- | Unwrap `Massiv` as a Delayed array.
unwrapMassiv :: Massiv ix e -> Array (Repr e) ix e
unwrapMassiv (Massiv arr) = arr
{-# INLINE [~1] unwrapMassiv #-}

-----------------------
-- Primitive data types
-----------------------

instance Index ix => Layout ix Char where
  type Repr Char = P

instance Index ix => Layout ix Double where
  type Repr Double = P

instance Index ix => Layout ix Float where
  type Repr Float = P

instance Index ix => Layout ix Int where
  type Repr Int = P

instance Index ix => Layout ix Int8 where
  type Repr Int8 = P

instance Index ix => Layout ix Int16 where
  type Repr Int16 = P

instance Index ix => Layout ix Int32 where
  type Repr Int32 = P

instance Index ix => Layout ix Int64 where
  type Repr Int64 = P

instance Index ix => Layout ix Word where
  type Repr Word = P

instance Index ix => Layout ix Word8 where
  type Repr Word8 = P

instance Index ix => Layout ix Word16 where
  type Repr Word16 = P

instance Index ix => Layout ix Word32 where
  type Repr Word32 = P

instance Index ix => Layout ix Word64 where
  type Repr Word64 = P

instance Index ix => Layout ix Addr where
  type Repr Addr = P


---------------------
-- Unboxed data types
---------------------

instance Index ix => Layout ix Bool where
  type Repr Bool = U

instance Index ix => Layout ix () where
  type Repr () = U

instance (RealFloat a, Mutable (Repr a /> U) ix (Complex a)) => Layout ix (Complex a) where
  type Repr (Complex a) = Repr a /> U

instance (Mutable (Repr a /> Repr b) ix (a, b)) => Layout ix (a, b) where
  type Repr (a, b) = Repr a /> Repr b

instance (Mutable (Repr a /> Repr b /> Repr c) ix (a, b, c)) =>
         Layout ix (a, b, c) where
  type Repr (a, b, c) = Repr a /> Repr b /> Repr c

instance (Mutable (Repr a /> Repr b /> Repr c /> Repr d) ix (a, b, c, d)) =>
         Layout ix (a, b, c, d) where
  type Repr (a, b, c, d) = Repr a /> Repr b /> Repr c /> Repr d

instance (Mutable (Repr a /> Repr b /> Repr c /> Repr d /> Repr e) ix (a, b, c, d, e)) =>
         Layout ix (a, b, c, d, e) where
  type Repr (a, b, c, d, e) = Repr a /> Repr b /> Repr c /> Repr d /> Repr e


instance Index ix => Layout ix Ix2 where
  type Repr Ix2 = U

instance (3 <= n, Unbox (Ix (n - 1)), Index ix) => Layout ix (IxN n) where
  type Repr (IxN n) = U



----------------------
-- Storable data types
----------------------

instance Index ix => Layout ix (Ptr a) where
  type Repr (Ptr a) = S

instance Index ix => Layout ix (FunPtr a) where
  type Repr (FunPtr a) = S

instance Index ix => Layout ix IntPtr where
  type Repr IntPtr = S

instance Index ix => Layout ix WordPtr where
  type Repr WordPtr = S

instance Index ix => Layout ix (StablePtr a) where
  type Repr (StablePtr a) = S

instance (Storable a, Index ix) => Layout ix (Identity a) where
  type Repr (Identity a) = S

instance (Storable (Const k a), Index ix) => Layout ix (Const k a) where
  type Repr (Const k a) = S

instance Index ix => Layout ix Fingerprint where
  type Repr Fingerprint = S


instance Index ix => Layout ix CChar where
  type Repr CChar = S

instance Index ix => Layout ix CSChar where
  type Repr CSChar = S

instance Index ix => Layout ix CUChar where
  type Repr CUChar = S

instance Index ix => Layout ix CShort where
  type Repr CShort = S

instance Index ix => Layout ix CUShort where
  type Repr CUShort = S

instance Index ix => Layout ix CInt where
  type Repr CInt = S

instance Index ix => Layout ix CUInt where
  type Repr CUInt = S

instance Index ix => Layout ix CLong where
  type Repr CLong = S

instance Index ix => Layout ix CULong where
  type Repr CULong = S

instance Index ix => Layout ix CPtrdiff where
  type Repr CPtrdiff = S

instance Index ix => Layout ix CSize where
  type Repr CSize = S

instance Index ix => Layout ix CWchar where
  type Repr CWchar = S

instance Index ix => Layout ix CSigAtomic where
  type Repr CSigAtomic = S

instance Index ix => Layout ix CLLong where
  type Repr CLLong = S

instance Index ix => Layout ix CULLong where
  type Repr CULLong = S

instance Index ix => Layout ix CIntPtr where
  type Repr CIntPtr = S

instance Index ix => Layout ix CUIntPtr where
  type Repr CUIntPtr = S

instance Index ix => Layout ix CIntMax where
  type Repr CIntMax = S

instance Index ix => Layout ix CUIntMax where
  type Repr CUIntMax = S

instance Index ix => Layout ix CClock where
  type Repr CClock = S

instance Index ix => Layout ix CTime where
  type Repr CTime = S

instance Index ix => Layout ix CUSeconds where
  type Repr CUSeconds = S

instance Index ix => Layout ix CSUSeconds where
  type Repr CSUSeconds = S

instance Index ix => Layout ix CFloat where
  type Repr CFloat = S

instance Index ix => Layout ix CDouble where
  type Repr CDouble = S

#if MIN_VERSION_base(4,10,0)

instance Index ix => Layout ix CBool where
  type Repr CBool = S

#endif


-------------------
-- Boxed data types
-------------------

instance (NFData e, Index ix) => Layout ix (Maybe e)

instance (NFData e, NFData a, Index ix) => Layout ix (Either e a)

instance Index ix => Layout ix Integer

instance (NFData a, Index ix) => Layout ix [a]

instance Index ix => Layout ix Rational

instance Index ix => Layout ix (a -> b)
