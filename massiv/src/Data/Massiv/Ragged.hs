{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Ragged
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Ragged where

import           Control.Monad       (unless)
import           Data.Massiv.Core
import qualified Data.Vector         as VB
import qualified Data.Vector.Unboxed as VU
import           GHC.Exts
import           GHC.TypeLits
import           Control.Monad.ST                   (runST)


type family BaseRag (v :: * -> *) :: * -> *

type instance BaseRag [] = Ragged [] 1
type instance BaseRag VB.Vector = Ragged VU.Vector 1

type family Rag (v :: * -> *) (n :: Nat) e where
  Rag v 1 e = e
  Rag v 2 e = Ragged (BaseDeep v) 1 e
  Rag v n e = Ragged v (n - 1) e

newtype Ragged (v :: * -> *) (n :: Nat) e = Ragged { unRagged :: v (Rag v n e) }

instance IsList (Ragged [] n e) where
  type Item (Ragged [] n e) = Rag [] n e
  fromList = Ragged
  toList = unRagged

type family BaseDeep (v :: * -> *) :: * -> *
type instance BaseDeep [] = []
type instance BaseDeep VB.Vector = VU.Vector


type family Deep (v :: * -> *) (n :: Nat) e where
  Deep v 0 e = e
  Deep v 1 e = BaseDeep v e
  Deep v n e = v (Deep v (n - 1) e)


type family NestedItem (v :: * -> *) (n :: Nat) e where
  NestedItem v 1 e = e
  NestedItem v 2 e = BaseDeep v e
  NestedItem v n e = v (NestedItem v (n - 1) e)


class Nested v n e where

  isNull :: Ragged v n e -> Bool
  default isNull :: (Foldable v) => Ragged v n e -> Bool
  isNull (Ragged xs) = null xs

  uncons :: Ragged v n e -> Maybe (Rag v n e, Ragged v n e)

  nestedSz :: Ragged v n e -> Ix n
  default nestedSz :: (Foldable v, n ~ 1) => Ragged v n e -> Int
  nestedSz = nestedLength

  nestedLength :: Ragged v n e -> Int
  default nestedLength :: Foldable v => Ragged v n e -> Int
  nestedLength (Ragged xs) = length xs

  nestedToRagged :: v (NestedItem v n e) -> Ragged v n e

  deepToRagged :: Deep v n e -> Ragged v n e
  -- default deepToRagged :: (n ~ 1) => Deep v n e -> Ragged v n e
  -- deepToRagged = Ragged

  loadNested ::
    (Monad m) =>
    (m () -> m ()) -> (Int -> e -> m a) -> Int -> Int -> Lower (Ix n) -> Ragged v n e -> m ()
  default loadNested :: (n ~ 1, Monad m) =>
            (m () -> m ()) -> (Int -> e -> m a) -> Int -> Int -> ix -> Ragged v n e -> m ()
  loadNested using uWrite start end _ xs = using $ do
    _leftOver <- loopM start (< end) (+ 1) xs $ \ i xs' ->
      case uncons xs' of
        Nothing      -> error $ "Row is too short"
        Just (y, ys) -> uWrite i y >> return ys
    --unless (P.null leftOver) $ error "Row is too long"
    return ()
  {-# INLINE loadNested #-}

-- (.>) :: Nested v n e => Ragged v n e -> Int -> Rag v n e
-- (.>) v i = unsafeIx

instance {-# OVERLAPPING #-} Nested [] 1 e where
  uncons (Ragged [])     = Nothing
  uncons (Ragged (x:xs)) = Just (x, Ragged xs)

  deepToRagged = Ragged

  nestedToRagged = Ragged


instance {-# OVERLAPPING #-} Nested [] 2 e where
  uncons (Ragged []) = Nothing
  uncons (Ragged (x:xs)) = Just (x, Ragged xs)

  deepToRagged xs = Ragged $ map deepToRagged xs

  nestedToRagged xs = Ragged $ map nestedToRagged xs

  nestedSz xs =
    nestedLength xs :.
    case uncons xs of
      Nothing -> zeroIndex
      Just (x, _) -> nestedSz x

  loadNested using uWrite = loadNestedRec (loadNested using uWrite)
  {-# INLINE loadNested #-}


-- instance {-# OVERLAPPING #-} Nested [] 3 e where
--   uncons (Ragged []) = Nothing
--   uncons (Ragged (x:xs)) = Just (x, Ragged xs)

--   deepToRagged xs = Ragged $ map deepToRagged xs

--   nestedToRagged xs = Ragged $ map nestedToRagged xs

--   nestedSz xs =
--     nestedLength xs :>
--     case uncons xs of
--       Nothing -> zeroIndex
--       Just (x, _) -> nestedSz x

--   loadNested using uWrite = loadNestedRec (loadNested using uWrite)
--   {-# INLINE loadNested #-}


instance ( Lower (Ix n) ~ Ix (n - 1)
         , Index (Lower (Ix n))
         , Index (Ix n)
         , Nested [] (n - 1) e
         , Ragged [] (n - 1) e ~ Rag [] n e
         , Deep [] n e ~ [Deep [] (n - 1) e]
         , NestedItem [] n e ~ [NestedItem [] (n - 1) e]
         , Ix n ~ IxN n
         , 4 <= n
         ) =>
         Nested [] n e where
  uncons (Ragged []) = Nothing
  uncons (Ragged (x:xs)) = Just (x, Ragged xs)

  deepToRagged xs = Ragged $ map deepToRagged xs

  nestedToRagged xs = Ragged $ map nestedToRagged xs

  nestedSz xs =
    nestedLength xs :>
    case uncons xs of
      Nothing -> zeroIndex
      Just (x, _) -> nestedSz x

  loadNested using uWrite = loadNestedRec (loadNested using uWrite)
  {-# INLINE loadNested #-}


loadNestedRec :: (Ragged v (n-1) e ~ Rag v n e, Index (Lower (Ix n)), Monad m, Nested v n e) =>
                  (Int -> Int -> Lower (Lower (Ix n)) -> Ragged v (n - 1) e -> m ())
               -> Int -> Int -> Lower (Ix n) -> Ragged v n e -> m ()
loadNestedRec loadLower start end sz xs = do
  let step = totalElem sz
      szL = snd (unconsDim sz)
  leftOver <-
    loopM start (< end) (+ step) xs $ \ !i zs ->
      case uncons zs of
        Nothing -> error "Too short"
        Just (y, ys) -> do
          loadLower i (i + step) szL y
          return ys
  unless (isNull leftOver) $ error "Too long"
{-# INLINE loadNestedRec #-}



fromRaggedS :: (Nested v n e, Mutable r (Ix n) e) => Ragged v n e -> Array r (Ix n) e
fromRaggedS xs =
  runST $ do
    let sz = nestedSz xs
    mArr <- unsafeNew sz
    loadNested id (unsafeLinearWrite mArr) 0 (totalElem sz) (tailDim sz) xs
    unsafeFreeze Seq mArr
{-# INLINE fromRaggedS #-}


fromRaggedS' :: (Nested v n e, IxN n ~ Ix n, Mutable r (IxN n) e) => Ragged v n e -> Array r (IxN n) e
fromRaggedS' xs =
  runST $ do
    let sz = nestedSz xs
    mArr <- unsafeNew sz
    loadNested id (unsafeLinearWrite mArr) 0 (totalElem sz) (tailDim sz) xs
    unsafeFreeze Seq mArr





-- loadListUsingN :: (Index ix, Monad m) =>
--                   (Int -> Int -> Lower ix -> e -> m ()) -> Int -> Int -> ix -> [e] -> m ()
-- loadListUsingN loadLower start end sz xs = do
--   let step = totalElem sz
--       szL = snd (unconsDim sz)
--   leftOver <-
--     loopM start (< end) (+ step) xs $ \ !i zs ->
--       case zs of
--         [] -> error "Too short"
--         (y:ys) -> do
--           loadLower i (i + step) szL y
--           return ys
--   unless (null leftOver) $ error "Too long"
-- {-# INLINE loadListUsingN #-}




-- instance (Rag [] n e ~ Ragged [] (n - 1) e, 3 <= n) => Iliffe (Ragged [] n) e where
--   nil = Ragged []

--   cons x (Ragged xs) = Ragged (x:xs)

--   uncons (Ragged [])     = Nothing
--   uncons (Ragged (x:xs)) = Just (x, Ragged xs)

--   unsafeIx (Ragged ls) = (ls !!)

--   loadListUsingN = loadListUsingNRec


-- instance Eq (Rag v n e) => Eq (Ragged v n e) where
--   (==) = size && zipWith (==)

-- instance IsList (Ragged [] n e) where
--   type Item (Ragged [] n e) = Rag [] n e
--   fromList = Ragged
--   toList = unRagged

-- instance VU.Unbox e => IsList (Ragged VU.Vector 1 e) where
--   type Item (Ragged VU.Vector 1 e) = e
--   fromList = Ragged . fromList
--   toList = toList . unRagged


-- instance IsList (Ragged VB.Vector n e) where
--   type Item (Ragged VB.Vector n e) = Rag VB.Vector n e
--   fromList = Ragged . fromList
--   toList = toList . unRagged


-- class Iliffe v e where
--   unsafeIx :: v e -> Int -> Lower (v e)

--   nil :: v e

--   cons :: Lower (v e) -> v e -> v e

--   uncons :: v e -> Maybe (Lower (v e), v e)

--   loadListUsingN :: (Index ix, Monad m) =>
--                     Int -> Int -> ix -> t -> (m () -> m ()) -> v e -> m ()

--   --makeRagged :: Rag v n Int -> (Rag v n Int -> e) -> Ragged v n e

-- (!>) :: Iliffe v e => v e -> Int -> Lower (v e)
-- (!>) = unsafeIx


-- instance Iliffe (Ragged [] 1) e where
--   nil = Ragged []

--   cons x (Ragged xs) = Ragged (x:xs)

--   uncons (Ragged [])     = Nothing
--   uncons (Ragged (x:xs)) = Just (x, Ragged xs)

--   unsafeIx (Ragged ls) = (ls !!)



-- instance (Rag [] n e ~ Ragged [] (n - 1) e, 3 <= n) => Iliffe (Ragged [] n) e where
--   nil = Ragged []

--   cons x (Ragged xs) = Ragged (x:xs)

--   uncons (Ragged [])     = Nothing
--   uncons (Ragged (x:xs)) = Just (x, Ragged xs)

--   unsafeIx (Ragged ls) = (ls !!)

--   loadListUsingN = loadListUsingNRec


-- type instance Lower (Ragged [] n e) = Rag [] n e

-- loadListUsingNRec
--   :: (3 <= (n - 1), 3 <= n, Index ix, Index (Lower ix), Monad m,
--       Ragged [] (n - 1) e ~ Lower (Ragged [] n e), Rag [] (n - 1) e ~ Ragged [] ((n - 1) - 1) e) =>
--      Int -> Int -> ix -> t -> (m () -> m ()) -> Ragged [] n e -> m ()
-- loadListUsingNRec start end sz uWrite using xs = do
--   let step = totalElem sz
--       szL = tailDim sz
--   _leftOver <-
--     loopM start (< end) (+ step) xs $ \ !i zs ->
--       case uncons zs of
--         Nothing -> error "Too short"
--         Just (y, ys) -> do
--           loadListUsingN i (i + step) szL uWrite using y
--           return ys
--     -- unless (nil == leftOver) $ error "Too long"
--   return ()


-- type instance Lower [e] = e

-- instance Iliffe [] where
--   nil = []

--   uncons []     = Nothing
--   uncons (x:xs) = Just (x, xs)

--   loadListUsingN start end sz uWrite using xs = do
--     let step = totalElem sz
--         szL = tailDim sz
--     _leftOver <-
--       loopM start (< end) (+ step) xs $ \ !i zs ->
--         case uncons zs of
--           Nothing -> error "Too short"
--           Just (y, ys) -> do
--             loadListUsingN i (i + step) szL uWrite using y
--             return ys
--     -- unless (nil == leftOver) $ error "Too long"
--     return ()
--   {-# INLINE loadListUsingN #-}



-- instance VU.Unbox (Rag VU.Vector 1 e) => Iliffe VU.Vector 1 e where
--   type SuperClass VU.Vector 1 e = VU.Unbox e
--   nil = Ragged VU.empty

--   cons x (Ragged v) = Ragged (VU.cons x v)

--   uncons (Ragged v) | VU.null v = Nothing
--                     | otherwise = Just (VU.head v, Ragged (VU.tail v))

--   unsafeIx (Ragged v) i = (v VU.! i)

--   mapR f (Ragged v) = Ragged (VU.map f v)

--   mapE = mapR

-- instance (2 <= n) => Iliffe VB.Vector n e where
--   --type SuperClass VB.Vector n e = Rag VB.Vector n e ~ Ragged VB.Vector (n-1) e

--   nil = Ragged VB.empty

--   cons x (Ragged xs) = Ragged (VB.cons x xs)

--   uncons (Ragged v) | VB.null v = Nothing
--                     | otherwise = Just (VB.head v, Ragged (VB.tail v))

--   unsafeIx (Ragged v) i = (v VB.! i)

--   mapR f (Ragged v) = Ragged (VB.map f v)

--   loadListUsingN start end sz uWrite using xs = do
--     let step = totalElem sz
--         szL = tailDim sz
--     _leftOver <-
--       loopM start (< end) (+ step) xs $ \ !i zs ->
--         case uncons zs of
--           Nothing -> error "Too short"
--           Just (y, ys) -> do
--             loadListUsingN i (i + step) szL uWrite using y
--             return ys
--     -- unless (nil == leftOver) $ error "Too long"
--     return ()
--   {-# INLINE loadListUsingN #-}



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
