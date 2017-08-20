{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- |
-- Module      : Data.Array.Massiv
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv
  (
    -- * Intro
    --
    -- Massiv (Масси́в) is a Russian word for an array, so everywhere you see
    -- this word you can safely assume that it is what its intended meaning. It
    -- is pronounced like English word "massive", except with an accent on
    -- _i_. There is also a data type family `Array` in this library, which is a
    -- more general way to deal with multi-dimnesional arrays. The benefit of
    -- using the wrapper `Massiv` data type instead, is that it will
    -- automatically handle fusion and deal with underlying representation for
    -- you, while with `Array` you have to keep track of it manually.
  ) where
import           Control.DeepSeq                (NFData)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Ix
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Mutable
import qualified Data.Array.Massiv.Ops          as A
import           Prelude                        as P hiding (length, map, mapM_,
                                                              null, unzip, unzip3, zip,
                                                              zip3, zipWith, zipWith3)
import Data.Functor.Identity
import GHC.Exts
import Data.Typeable
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Generic as VG
-- import qualified Data.Vector.Unboxed as VU

data Massiv ix e = Target (Repr e) ix e => Massiv !(Array (Repr e) ix e)



type family If (pred :: Bool) (thenB :: Constraint) (elseB :: Constraint) :: Constraint where
  If 'True t r = t
  If 'False t r = r

type family Or (l :: Bool) (r :: Bool) :: Bool where
  Or 'True a = 'True
  Or 'False 'True = 'True
  Or 'False 'False = 'False

type family And (l :: Bool) (r :: Bool) :: Bool where
  And 'True  'True  = 'True
  And 'False 'True  = 'False
  And 'True  'False = 'False
  And 'False 'False = 'False

-- type family Repr' v a where
--   Repr' VU.Vector a = U
--   Repr' V.Vector  a = B

type family IsUnbox t :: Bool where
  IsUnbox Bool = 'True
  IsUnbox Int = 'True
  IsUnbox (a, b) = And (IsUnbox a) (IsUnbox b)
  -- IsUnbox a = ShouldUnbox 'Nothing
  -- IsUnbox a = ShouldUnbox ('Just a)
  IsUnbox a = 'False

type family ShouldUnbox (a :: Maybe b) :: Bool

type instance ShouldUnbox ('Just Int) = 'True
type instance ShouldUnbox ('Just (Maybe a)) = 'False
type instance ShouldUnbox ('Just (a, b)) = And (ShouldUnbox ('Just a)) (ShouldUnbox ('Just b))
type instance ShouldUnbox ('Nothing) = 'False



type family Elem t ls :: Bool where
  Elem t '[] = 'False
  Elem t (t ': xs) = 'True
  Elem t (t' ': xs) = Elem t xs

type family Custom u :: [*]

type instance Custom U = [Double, Float]

type family Res t where
  Res 'True = U
  Res 'False = B


computeM
  :: forall e ix r.
     (Load r ix e, Target (Repr e) ix e)
  => Array r ix e -> Massiv ix e
computeM = Massiv . compute
{-# INLINE [1] computeM #-}

--type DoUnbox e = (IsUnbox e) (IsUnbox (Foo e)))

type Repr e = (Res (IsUnbox e))

--type Layout ix e = (If (Or (IsUnbox e) (ShouldUnbox e)) (Target U ix e) (Target B ix e))
--type Layout ix e = () --Target (Res (IsUnbox e)) ix e)

-- class (Mine r ix e, Target r ix e) => Layout r ix e where

-- instance (Mine r ix e, Target r ix e) => Layout r ix e

-- computeM :: forall r' r ix e . (Load r ix e, Layout r' ix e) => Array r ix e -> Massiv ix e
-- computeM = Massiv . computeAs (undefined :: r')
-- {-# INLINE [1] computeM #-}

delayM :: Massiv ix e -> Array D ix e
delayM (Massiv arr) = delay arr
{-# INLINE [1] delayM #-}

getOut :: Massiv ix e -> Array (Repr e) ix e
getOut (Massiv arr) = arr
-- makeMassiv
--   :: (Layout r' ix e) =>
--      Comp -> ix -> (ix -> e) -> Massiv ix e
-- makeMassiv
--   :: (Res (IsUnbox e) ~ r, Target r ix e) =>
--      Comp -> ix -> (ix -> e) -> Massiv ix e
makeMassiv
  :: Target (Repr e) ix e =>
     Comp -> ix -> (ix -> e) -> Massiv ix e
makeMassiv c sz f = computeM (A.makeArrayR D c sz f)
{-# INLINE [~1] makeMassiv #-}

-- -- -- | Map a function over Massiv
-- map :: (Layout r' ix e, Layout r' ix e') => (e' -> e) -> Massiv ix e' -> Massiv ix e
map ::  Target (Repr e) ix e => (e' -> e) -> Massiv ix e' -> Massiv ix e
map f = computeM . A.map f . delayM
{-# INLINE [~1] map #-}


ix :: Massiv ix t -> ix -> t
ix (Massiv arr) i = arr ! i



-- class a || b where
--   resolve :: (a => r) -> (b => r) -> r

-- instance b => (Storable a || b) where
--   resolve = \_ b -> b

-- instance Storable a => (Storable a || NFData b) where
--   resolve = \s _ -> s

-- instance a => (Unbox e || a) where
--   resolve = \_ c -> c

-- instance {-# OVERLAPPING #-} (NFData a, NFData b) => (NFData (a, b) || d) where
--   resolve = \a _ -> a

-- instance {-# OVERLAPPING #-} (Unbox Int || a) where
--   resolve = \u _ -> u

-- instance {-# OVERLAPPING #-} (Unbox a, Unbox b) => (Unbox (a, b) || d) where
--   resolve = \a _ -> a


-- instance NFData a => (NFData a || b) where
--   resolve = \b _ -> b


-- instance Unbox a => (Unbox a || d) where
--   resolve = \b _ -> b


-- instance  {-# OVERLAPPING #-} (Unbox a, NFData a) => ((Unbox a, NFData a) || d) where
--   resolve = \a _ -> a



-- instance {-# OVERLAPPING #-} Unbox e => (Unbox e || (Storable e || NFData e)) where
--   resolve = \u _ -> u

-- instance {-# OVERLAPPING #-} Unbox a => (Unbox a || b) where
--   resolve = \c _ -> c


-- inLeft :: forall c d r. c => (c => r) -> (d => r) -> r
-- inLeft r _ = r

-- inRight :: forall c d r. d => (c => r) -> (d => r) -> r
-- inRight _ r = r

-- instance ((c0 || d), (c1 || d)) => (c0, c1) || d where
--   resolve = resolve @c0 @d (resolve @c1 @d inLeft inRight) inRight where

-- instance ((c0 || d), ((c1,c2) || d)) => (c0, c1, c2) || d where
--   resolve = resolve @c0 @d (resolve @(c1,c2) @d inLeft inRight) inRight where


-- instance (Storable (Identity Int) || b) where
--   resolve = \c _ -> c


-- instance {-# OVERLAPPING #-} ((NFData a, NFData b) || d) => (NFData (a, b) || d) where
--   resolve = resolve @(NFData a, NFData b) @d

-- instance {-# OVERLAPPING #-} (NFData a, NFData b) => ((NFData a, NFData b) || d) where
--   resolve = \a _ -> a

-- instance {-# OVERLAPPING #-} ((Unbox Int, Unbox Int) || d) where
--   resolve = \a _ -> a


-- instance {-# OVERLAPPING #-} ((Unbox a, Unbox b) || d) => (Unbox (a, b) || d) where
--   resolve = resolve @(Unbox a, Unbox b) @d


-- instance {-# OVERLAPPING #-} (Unbox Int || d) where
--   resolve = \c _ -> c

-- instance d => (Storable e || d) where
--   resolve = \_ c -> c

-- -- | Load an `Array` into memory as a `Massiv`
-- computeM :: forall r ix e. (Layout ix e, Load r ix e) => Array r ix e -> Massiv ix e
-- computeM = resolve @(Unbox e) @(Unbox e, NFData e) (Massiv . err U {- computeAs U -}) (Massiv . err B {- computeAs B -})
--   where
--     err :: Show r' => r' -> Array r ix e -> Array r' ix e
--     err r _ = error $ "Foo: " ++ show r
-- {-# INLINE [1] computeM #-}


-- class (Elt (Repr e) e, Target (Repr e) ix e) => Layout ix e where
--   type Repr e :: *
--   type Repr e = B

-- -- | Generate `Massiv` of a specified size using a function to creates it's
-- -- elements. All further computation on generated Massiv will be done according
-- -- the `Comp` strategy supplied.
-- makeMassiv :: Layout ix e =>
--               Comp -- ^ Computation strategy. Useful constructors are `Seq` and `Par`
--            -> ix -- ^ Size of the result `Massiv`
--            -> (ix -> e) -- ^ Function to generate elements for a every index in
--                         -- the result `Massiv`
--            -> Massiv ix e

-- -- | Zip two arrays
-- zip :: (Unbox a, Unbox b, Layout ix b, Layout ix a) =>
--        Massiv ix a -> Massiv ix b -> Massiv ix (a, b)
-- zip = zipWith (,)
-- {-# INLINE zip #-}

-- -- | Zip two arrays with a function. Resulting array will be an intersection of
-- -- source arrays in case their dimensions do not match.
-- zipWith :: (Layout ix e1, Layout ix e2, Layout ix e) =>
--             (e1 -> e2 -> e)
--          -> Massiv ix e1
--          -> Massiv ix e2
--          -> Massiv ix e
-- zipWith f a1 a2 = computeM $ A.zipWith f (delayM a1) (delayM a2)
-- {-# INLINE [~1] zipWith #-}
