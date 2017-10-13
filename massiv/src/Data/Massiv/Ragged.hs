{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Ragged
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Ragged where

import GHC.TypeLits
import GHC.Exts
import qualified Data.Vector as VG
import qualified Data.Vector.Unboxed as VU





newtype Ragged (v :: * -> *) (n :: Nat) e = Ragged { unRagged :: v (Rag v n e) }

instance IsList (Ragged [] n e) where
  type Item (Ragged [] n e) = Rag [] n e
  fromList = Ragged
  toList = unRagged

instance VU.Unbox e => IsList (Ragged VU.Vector 1 e) where
  type Item (Ragged VU.Vector 1 e) = e
  fromList = Ragged . fromList
  toList = toList . unRagged


instance IsList (Ragged VG.Vector n e) where
  type Item (Ragged VG.Vector n e) = Rag VG.Vector n e
  fromList = Ragged . fromList
  toList = toList . unRagged


class Vector v n e where
  unsafeIx :: Ragged v n e -> Int -> Rag v n e

  nil :: Ragged v n e

  cons :: Rag v n e -> Ragged v n e -> Ragged v n e

  mapE :: Vector v n e' => (Rag v n e' -> Rag v n e) -> Ragged v n e' -> Ragged v n e

  --makeRagged :: Rag v n Int -> (Rag v n Int -> e) -> Ragged v n e

(!>) :: Vector v n e => Ragged v n e -> Int -> Rag v n e
(!>) = unsafeIx

instance Vector [] n e where
  nil = Ragged []

  cons x (Ragged xs) = Ragged (x:xs)

  unsafeIx (Ragged ls) = (ls !!)



instance VU.Unbox (Rag VU.Vector 1 e) => Vector VU.Vector 1 e where
  nil = Ragged VU.empty

  cons x (Ragged xs) = Ragged (VU.cons x xs)

  unsafeIx (Ragged v) i = (v VU.! i)

  --mapE f (Ragged v) = Ragged (VU.map f v) -- <-- ghc bug ?

instance Vector VG.Vector 2 e where
  nil = Ragged VG.empty

  cons x (Ragged xs) = Ragged (VG.cons x xs)

  unsafeIx (Ragged v) i = (v VG.! i)

  mapE f (Ragged v) = Ragged (VG.map f v)






-- --ix :: Vector v (Rag v n e) => Ragged v n e -> Int -> Rag v n e
-- ix :: Vector v e => Ragged v t e -> Int -> e
-- ix (Ragged v) = unsafeIx v


type family BaseRag (v :: * -> *) :: * -> *

type instance BaseRag [] = Ragged [] 1
type instance BaseRag VG.Vector = Ragged VU.Vector 1

type family Rag (v :: * -> *) (n :: Nat) e where
  Rag v 1 e = e
  Rag v 2 e = BaseRag v e
  Rag v n e = Ragged v (n - 1) e


-- class RagVec v (n :: Nat) e where
--   type Rag v n e :: *
--   ix :: v e -> Int -> Rag v n e

--   len :: v e -> Int

--   uncons :: v e -> Maybe (Rag v n e, v e)

-- instance Ragged List 1 e where
--   ix (List ls) = (ls !!)

--   len (List ls) = length ls

-- instance (Rag List n (List n' e) ~ List n' e) => Ragged List n (List n' e) where
--   ix (List ls) = (ls !!)

--   len (List ls) = length ls




-- newtype List (n :: Nat) e = List [e]

-- type family Rag (arr :: Nat -> * -> *) (n :: Nat) e where
--   Rag arr 1 e = e
--   Rag arr n e = Rag arr (n - 1) e

-- class Ragged arr (n :: Nat) e where
--   ix :: arr n e -> Int -> Rag arr n e

--   len :: arr n e -> Int

--   uncons :: arr n e -> Maybe (e, arr n e)

-- instance Ragged List 1 e where
--   ix (List ls) = (ls !!)

--   len (List ls) = length ls

-- instance (Rag List n (List n' e) ~ List n' e) => Ragged List n (List n' e) where
--   ix (List ls) = (ls !!)

--   len (List ls) = length ls
