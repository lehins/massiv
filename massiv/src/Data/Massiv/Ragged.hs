{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

import           Control.Monad              (unless)
import           Control.Monad.ST           (runST)
import           Data.Coerce
import           Data.Functor.Identity
import qualified Data.List                  as L
import           Data.Massiv.Core
import           Data.Massiv.Core.Scheduler
-- import qualified Data.Vector                as VB
-- import qualified Data.Vector.Unboxed        as VU
-- import           GHC.Base                   (build)
import           GHC.Exts
import           GHC.TypeLits
import           System.IO.Unsafe           (unsafePerformIO)


data LN = LN

type family Elt r ix e :: *
type instance Elt LN Ix1 e = e
type instance Elt LN Ix2 e = Array LN Ix1 e
type instance Elt LN (IxN n) e = Array LN (Ix (n-1)) e

newtype instance Array LN ix e = List [Elt LN ix e]

-- type family Nested r ix e :: *
-- type instance Nested LN Ix1 e = e
-- type instance Nested LN Ix2 e = [e]
-- type instance Nested LN (IxN n) e = Nested LN (Ix (n-1)) e



data L = L

data instance Array L ix e = LArray { lComp :: Comp
                                    , lSize :: ix
                                    , lData :: !(Array LN ix e) }
type instance Elt L Ix1 e = e
type instance Elt L Ix2 e = Array L Ix1 e
type instance Elt L (IxN n) e = Array L (Ix (n-1)) e

-- instance (Index ix, Ragged LN ix e) => Construct L ix e where
--   size = lSize
--   {-# INLINE size #-}
--   getComp = lComp
--   {-# INLINE getComp #-}
--   setComp c arr = arr { lComp = c }
--   {-# INLINE setComp #-}
--   unsafeMakeArray comp sz f = LArray comp sz $ runIdentity $ unsafeGenerateM sz (return . f)
--   unsafeMakeArray comp@(ParOn wss) sz f =
--     LArray comp sz $ unsafePerformIO $ unsafeGenerateP' wss sz (return . f)
--   {-# INLINE unsafeMakeArray #-}


instance {-# OVERLAPPING #-} Construct L Ix1 e where
  size = lSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr { lComp = c }
  {-# INLINE setComp #-}
  -- TODO: load thunks in list in parallel as well.
  unsafeMakeArray comp sz f = LArray comp sz $ runIdentity $ unsafeGenerateM sz (return . f)
  {-# INLINE unsafeMakeArray #-}


instance ( Index ix
         , Ragged LN ix e
         , Ragged LN (Lower ix) e
         , Elt LN ix e ~ Array LN (Lower ix) e
         ) =>
         Construct L ix e where
  size = lSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr {lComp = c}
  {-# INLINE setComp #-}
  unsafeMakeArray comp sz f = LArray comp sz $ unsafeGenerateN comp sz f
  {-# INLINE unsafeMakeArray #-}

-- instance Construct L (IxN 3) e where
--   size = lSize
--   {-# INLINE size #-}
--   getComp = lComp
--   {-# INLINE getComp #-}
--   setComp c arr = arr { lComp = c }
--   {-# INLINE setComp #-}
--   unsafeMakeArray comp sz f = LArray comp sz $ unsafeGenerateN comp sz f
--   {-# INLINE unsafeMakeArray #-}


-- instance ( Ragged LN (Ix (n - 1)) e
--          , Index (Ix (n - 1))
--          , 4 <= n
--          , IxN (n - 1) ~ Ix (n - 1)
--          , KnownNat n
--          ) =>
--          Construct L (IxN n) e where
--   size = lSize
--   {-# INLINE size #-}
--   getComp = lComp
--   {-# INLINE getComp #-}
--   setComp c arr = arr {lComp = c}
--   {-# INLINE setComp #-}
--   unsafeMakeArray comp sz f = LArray comp sz $ unsafeGenerateN comp sz f
--   {-# INLINE unsafeMakeArray #-}


instance (Ragged L ix e, Construct L ix e, Index ix) => Load L ix e where
  loadS arr _ unsafeWrite =
    loadNested id unsafeWrite 0 (totalElem sz) (tailDim sz) arr
    where sz = nestedSz arr
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite =
    withScheduler_ wIds $ \scheduler ->
      loadNested (scheduleWork scheduler) unsafeWrite 0 (totalElem sz) (tailDim sz) arr
    where sz = nestedSz arr
  {-# INLINE loadP #-}


unsafeGenerateN ::
  ( Index ix
  , Ragged r ix e
  , Ragged r (Lower ix) e
  , Elt r ix e ~ Array r (Lower ix) e )
  => Comp
  -> ix
  -> (ix -> e)
  -> Array r ix e
unsafeGenerateN Seq sz f = runIdentity $ unsafeGenerateM sz (return . f)
unsafeGenerateN (ParOn wss) sz f = unsafePerformIO $ do
  let (m, szL) = unconsDim sz
  xs <- withScheduler' wss $ \scheduler -> do
    loopM_ 0 (< m) (+ 1) $ \i -> scheduleWork scheduler $ do
      unsafeGenerateM szL $ \ix -> return $ f (consDim i ix)
  return $ foldr cons empty xs
{-# INLINE unsafeGenerateN #-}


class Ragged r ix e where

  empty :: Array r ix e

  isNull :: Array r ix e -> Bool

  cons :: Elt r ix e -> Array r ix e -> Array r ix e

  uncons :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  unsafeGenerateM :: Monad m => ix -> (ix -> m e) -> m (Array r ix e)

  nestedSz :: Array r ix e -> ix

  nestedLength :: Array r ix e -> Int

  -- TODO: test property:
  -- (read $ raggedFormat show "\n" (ls :: Array L (IxN n) Int)) == ls
  raggedFormat :: (e -> String) -> String -> Array r ix e -> String

  loadNested ::
    (Monad m) =>
    (m () -> m ()) -> (Int -> e -> m a) -> Int -> Int -> Lower ix -> Array r ix e -> m ()


instance {-# OVERLAPPING #-} (Ragged L ix e, Show e) =>
  Show (Array L ix e) where
  show arr = raggedFormat show "\n" arr

instance {-# OVERLAPPING #-} (Ragged LN ix e, Show e) =>
  Show (Array LN ix e) where
  show arr = raggedFormat show "\n" arr

instance ( Ragged L ix e
         , Construct L ix e
         , Source r ix e
         , Show e
         ) =>
         Show (Array r ix e) where
  show arr =
    raggedFormat
      show
      "\n"
      (unsafeMakeArray (getComp arr) (size arr) (unsafeIndex arr) :: Array L ix e)


showN :: (String -> a -> String) -> String -> [a] -> String
showN fShow lnPrefix ls =
  L.concat
    (["[ "] ++
     (L.intersperse (lnPrefix ++ ", ") $ map (fShow (lnPrefix ++ "  ")) ls) ++ [lnPrefix, "]"])


instance {-# OVERLAPPING #-} Ragged L Ix1 e where
  isNull = isNull . lData
  {-# INLINE isNull #-}
  empty = LArray Seq zeroIndex empty
  {-# INLINE empty #-}
  nestedSz = nestedSz . lData
  {-# INLINE netsedSz #-}
  nestedLength = nestedLength . lData
  {-# INLINE nestedLength #-}
  cons x arr = newArr
    where
      newArr = arr {lData = cons x (lData arr), lSize = nestedSz newArr}
  {-# INLINE cons #-}
  uncons LArray {..} =
    case uncons lData of
      Nothing      -> Nothing
      Just (x, xs) -> Just (x, LArray lComp (nestedSz xs) xs)
  {-# INLINE uncons #-}
  unsafeGenerateM = unsafeGenerateLArrayM
  {-# INLINE cons #-}
  loadNested using uWrite start end szL arr =
    loadNested using uWrite start end szL (lData arr)
  raggedFormat f sep arr = raggedFormat f sep (lData arr)

instance ( Index ix
         , Ragged LN ix e
         , Ragged LN (Lower ix) e
         , Elt LN ix e ~ Array LN (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         ) =>
         Ragged L ix e where
  isNull = isNull . lData
  {-# INLINE isNull #-}
  empty = LArray Seq zeroIndex empty
  {-# INLINE empty #-}
  nestedSz = nestedSz . lData
  {-# INLINE nestedSz #-}
  nestedLength = nestedLength . lData
  {-# INLINE nestedLength #-}
  cons = consLArray
  {-# INLINE cons #-}
  uncons = unconsLArray
  {-# INLINE uncons #-}
  unsafeGenerateM = unsafeGenerateLArrayM
  {-# INLINE unsafeGenerateM #-}
  loadNested using uWrite start end szL arr =
    loadNested using uWrite start end szL (lData arr)
  {-# INLINE loadNested #-}
  raggedFormat f sep arr = raggedFormat f sep (lData arr)
  {-# INLINE raggedFormat #-}


-- instance Ragged L Ix2 e where
--   isNull = isNull . lData
--   empty = LArray Seq zeroIndex empty
--   nestedSz = nestedSz . lData
--   nestedLength = nestedLength . lData
--   cons = consLArray
--   uncons = unconsLArray
--   unsafeGenerateM = unsafeGenerateLArrayM
--   loadNested using uWrite start end szL arr =
--     loadNested using uWrite start end szL (lData arr)
--   raggedFormat f sep arr = raggedFormat f sep (lData arr)

-- instance Ragged L Ix3 e where
--   isNull = isNull . lData
--   empty = LArray Seq zeroIndex empty
--   nestedSz = nestedSz . lData
--   nestedLength = nestedLength . lData
--   cons = consLArray
--   uncons = unconsLArray
--   unsafeGenerateM = unsafeGenerateLArrayM
--   loadNested using uWrite start end szL arr =
--     loadNested using uWrite start end szL (lData arr)
--   raggedFormat f sep arr = raggedFormat f sep (lData arr)


-- instance ( Index (Ix (n - 1))
--          , Ragged LN (Ix (n - 1)) e
--          , 4 <= n
--          , KnownNat n
--          , IxN (n - 1) ~ Ix (n - 1)
--          ) =>
--          Ragged L (IxN n) e where
--   isNull = isNull . lData
--   empty = LArray Seq zeroIndex empty
--   nestedSz = nestedSz . lData
--   nestedLength = nestedLength . lData
--   cons = consLArray
--   uncons = unconsLArray
--   unsafeGenerateM = unsafeGenerateLArrayM
--   loadNested using uWrite start end szL arr =
--     loadNested using uWrite start end szL (lData arr)
--   raggedFormat f sep arr = raggedFormat f sep (lData arr)


unsafeGenerateLArrayM :: (Monad m, Ragged LN ix e) =>
                         ix -> (ix -> m e) -> m (Array L ix e)
unsafeGenerateLArrayM k f = do
  genData <- unsafeGenerateM k f
  return $ LArray Seq (nestedSz genData) genData
{-# INLINE unsafeGenerateLArrayM #-}

consLArray
  :: (Elt LN ix e ~ Array LN ix1 e1, Ragged LN ix e) =>
     Array L ix1 e1 -> Array L ix e -> Array L ix e
consLArray x arr = arr {lData = newData, lSize = nestedSz newData}
  where
    newData = cons (lData x) (lData arr)
{-# INLINE consLArray #-}

unconsLArray
  :: (Elt LN ix1 e1 ~ Array LN ix e, Ragged LN ix1 e1,
      Ragged LN ix e) =>
     Array L ix1 e1 -> Maybe (Array L ix e, Array L ix1 e1)
unconsLArray arr@LArray {..} =
  case uncons lData of
    Nothing -> Nothing
    Just (x, newData) ->
      Just
        ( LArray lComp (nestedSz x) x
        , arr {lData = newData, lSize = nestedSz newData})
{-# INLINE unconsLArray #-}

instance {-# OVERLAPPING #-} Ragged LN Ix1 e where
  isNull (List xs) = null xs
  {-# INLINE isNull #-}
  empty = List []
  {-# INLINE empty #-}
  nestedSz = nestedLength
  {-# INLINE nestedSz #-}
  nestedLength (List xs) = length xs
  {-# INLINE nestedLength #-}
  cons x (List xs) = List (x : xs)
  {-# INLINE cons #-}
  uncons (List []) = Nothing
  uncons (List (x:xs)) = Just (x, List xs)
  {-# INLINE uncons #-}
  unsafeGenerateM k f =
    loopM (k - 1) (>= 0) (subtract 1) empty $ \i acc -> do
      e <- f i
      return $ cons e acc
  {-# INLINE unsafeGenerateM #-}
  loadNested using uWrite start end _ xs =
    using $ do
      _leftOver <-
        loopM start (< end) (+ 1) xs $ \i xs' ->
          case uncons xs' of
            Nothing -> error $ "Row is too short"
            Just (y, ys) -> uWrite i y >> return ys
      --unless (isNull leftOver) $ error "Row is too long"
      return ()
  {-# INLINE loadNested #-}
  raggedFormat f _ (List xs) = L.concat $ "[ " : (L.intersperse "," $ map f xs) ++ [" ]"]

instance ( Index ix
         , Index (Lower ix)
         , Ragged LN (Lower ix) e
         , Elt LN ix e ~ Array LN (Lower ix) e
         ) =>
         Ragged LN ix e where
  isNull (List xs) = null xs
  {-# INLINE isNull #-}
  empty = List []
  {-# INLINE empty #-}
  nestedSz xs =
    consDim (nestedLength xs) $
    case uncons xs of
      Nothing -> zeroIndex
      Just (x, _) -> nestedSz x
  {-# INLINE nestedSz #-}
  nestedLength (List xs) = length xs
  {-# INLINE nestedLength #-}
  cons x (List xs) = List (x : xs)
  {-# INLINE cons #-}
  uncons (List []) = Nothing
  uncons (List (x:xs)) = Just (x, List xs)
  {-# INLINE uncons #-}
  loadNested using uWrite = loadNestedRec (loadNested using uWrite)
  {-# INLINE loadNested #-}
  unsafeGenerateM sz f = do
    let (k, szL) = unconsDim sz
    loopM (k - 1) (>= 0) (subtract 1) empty $ \i acc -> do
      e <- unsafeGenerateM szL (\ixL -> f (consDim i ixL))
      return $ cons e acc
  {-# INLINE unsafeGenerateM #-}
  raggedFormat f sep (List xs) = showN (raggedFormat f) sep xs

-- instance Ragged LN Ix2 e where
--   isNull (List xs) = null xs
--   empty = List []
--   nestedSz xs =
--     nestedLength xs :.
--     case uncons xs of
--       Nothing -> zeroIndex
--       Just (x, _) -> nestedSz x
--   nestedLength (List xs) = length xs
--   cons x (List xs) = List (x : xs)
--   uncons (List []) = Nothing
--   uncons (List (x:xs)) = Just (x, List xs)
--   unsafeGenerateM (m :. n) f = do
--     loopM (m - 1) (>= 0) (subtract 1) empty $ \i acc -> do
--       e <- unsafeGenerateM n (\j -> f (i :. j))
--       return $ cons e acc
--   loadNested using uWrite = loadNestedRec (loadNested using uWrite)
--   {-# INLINE loadNested #-}
--   raggedFormat f sep (List xs) = showN (raggedFormat f) sep xs


-- instance (Index (Ix (n - 1)), Ragged LN (Ix (n - 1)) e) =>
--          Ragged LN (IxN n) e where
--   isNull (List xs) = null xs
--   empty = List []
--   nestedSz xs =
--     nestedLength xs :>
--     case uncons xs of
--       Nothing -> zeroIndex
--       Just (x, _) -> nestedSz x
--   nestedLength (List xs) = length xs
--   cons x (List xs) = List (x : xs)
--   uncons (List []) = Nothing
--   uncons (List (x:xs)) = Just (x, List xs)
--   loadNested using uWrite = loadNestedRec (loadNested using uWrite)
--   {-# INLINE loadNested #-}
--   unsafeGenerateM (k :> szL) f = do
--     loopM (k - 1) (>= 0) (subtract 1) empty $ \i acc -> do
--       e <- unsafeGenerateM szL (\ixL -> f (i :> ixL))
--       return $ cons e acc
--   raggedFormat f sep (List xs) = showN (raggedFormat f) sep xs



-- instance {-# OVERLAPPING #-} IsList (Array L Ix1 e) where
--   type Item (Array L Ix1 e) = Item (Array LN Ix1 e)
--   fromList xs = LArray Seq (nestedSz ls) ls
--     where ls = fromList xs :: Array LN Ix1 e
--   toList = toList . lData

instance (Ragged LN ix e, IsList (Array LN ix e)) => IsList (Array L ix e) where
  type Item (Array L ix e) = Item (Array LN ix e)
  fromList xs = LArray Seq (nestedSz ls) ls
    where
      ls = fromList xs :: Array LN ix e
  {-# INLINE fromList #-}
  toList = toList . lData
  {-# INLINE toList #-}

-- instance IsList (Array L Ix2 e) where
--   type Item (Array L Ix2 e) = Item (Array LN Ix2 e)
--   fromList xs = LArray Seq (nestedSz ls) ls
--     where ls = fromList xs :: Array LN Ix2 e
--   toList = toList . lData

-- instance ( Elt LN (Ix (n - 1)) e ~ Item (Array LN (Ix (n - 1)) e)
--          , IsList (Array LN (Ix (n - 1)) e)
--          , Ragged LN (Ix (n - 1)) e
--          , Index (Ix (n - 1))
--          ) =>
--          IsList (Array L (IxN n) e) where
--   type Item (Array L (IxN n) e) = Item (Array LN (IxN n) e)
--   fromList xs = LArray Seq (nestedSz ls) ls
--     where ls = fromList xs :: Array LN (IxN n) e
--   toList = toList . lData


instance IsList (Array LN Ix1 e) where
  type Item (Array LN Ix1 e) = e
  fromList = coerce
  toList = coerce


instance IsList (Array LN Ix2 e) where
  type Item (Array LN Ix2 e) = [e]
  fromList = coerce
  {-# INLINE fromList #-}
  toList = coerce
  {-# INLINE toList #-}


instance IsList (Array LN (IxN n) e) where
  type Item (Array LN (IxN n) e) = Elt LN (IxN n) e
  fromList = coerce
  --fromList = List . map fromList
  toList = coerce
  --toList (List xs) = map toList xs


loadNestedRec :: (Index ix1, Monad m, Ragged r ix e) =>
                 (Int -> Int -> Lower ix1 -> Elt r ix e -> m a)
              -> Int -> Int -> ix1 -> Array r ix e -> m ()
loadNestedRec loadLower start end sz xs = do
  let step = totalElem sz
      szL = snd (unconsDim sz)
  _leftOver <-
    loopM start (< end) (+ step) xs $ \ i zs ->
      case uncons zs of
        Nothing -> error "Too short"
        Just (y, ys) -> do
          _ <- loadLower i (i + step) szL y
          return ys
  --unless (isNull leftOver) $ error "Too long"
  return ()
{-# INLINE loadNestedRec #-}


fromRaggedS :: (Ragged r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
fromRaggedS xs =
  runST $ do
    let sz = nestedSz xs
    mArr <- unsafeNew sz
    loadNested id (unsafeLinearWrite mArr) 0 (totalElem sz) (tailDim sz) xs
    unsafeFreeze Seq mArr
{-# INLINE fromRaggedS #-}

toListArray ::
     forall r ix e. (IsList (Array L ix e), Construct L ix e, Source r ix e)
  => Array r ix e
  -> [Item (Array L ix e)]
toListArray arr =
  toList
    (unsafeMakeArray (getComp arr) (size arr) (unsafeIndex arr) :: Array L ix e)
{-# INLINE toListArray #-}


-- -- | Version of foldr that supports foldr/build list fusion implemented by GHC.
-- foldrFB :: (e -> b -> b) -> b -> Int -> (Int -> e) -> b
-- --foldrFB c n k f = loop (k - 1) (>= 0) (subtract 1) n $ \i acc -> f i `c` acc
-- foldrFB c n k f = go 0
--   where
--     go !i
--       | i == k = n
--       | otherwise = let !v = f i in v `c` go (i + 1)
-- {-# INLINE [0] foldrFB #-}
