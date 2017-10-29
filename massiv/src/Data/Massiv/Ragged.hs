{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Ragged
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--

-- Summary:
-- L:
--   * unsafeGenerateM
--   * isNull
-- LN:
--   * isNull

module Data.Massiv.Ragged () where

import           Control.Monad              (unless)
import           Data.Coerce
import           Data.Foldable              (foldr')
import           Data.Functor.Identity
import qualified Data.List                  as L
import           Data.Massiv.Core
import           Data.Massiv.Core.Scheduler
import           GHC.Exts
import           System.IO.Unsafe           (unsafePerformIO)


data LN = LN
type instance EltRepr LN ix = LN


type family ListItem ix e :: * where
  ListItem Ix1 e = e
  ListItem ix  e = [ListItem (Lower ix) e]


newtype instance Array LN ix e = List { unList :: [Elt LN ix e] }

data L = L
type instance EltRepr L ix = L

data instance Array L ix e = LArray { lComp :: Comp
                                    , lSize :: ix
                                    , lData :: !(Array LN ix e) }

instance {-# OVERLAPPING #-} Construct L Ix1 e where
  size = lSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr { lComp = c }
  {-# INLINE setComp #-}
  unsafeMakeArray Seq sz f = LArray Seq sz $ runIdentity $ unsafeGenerateM sz (return . f)
  unsafeMakeArray (ParOn wss) sz f = LArray (ParOn wss) sz $ List $ unsafePerformIO $ do
    withScheduler' wss $ \scheduler ->
      loopM_ 0 (< sz) (+ 1) (scheduleWork scheduler . return . f)
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


instance (Ragged LN ix e, Construct L ix e, Index ix) => Load L ix e where
  loadS (LArray _ sz xs) _ unsafeWrite =
    loadNested id unsafeWrite 0 (totalElem sz) (tailDim sz) xs
  {-# INLINE loadS #-}
  loadP wIds (LArray _ sz xs) _ unsafeWrite =
    withScheduler_ wIds $ \scheduler ->
      loadNested (scheduleWork scheduler) unsafeWrite 0 (totalElem sz) (tailDim sz) xs
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
  return $! foldr' cons empty xs
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


  fromListIx :: [ListItem ix e] -> Array r ix e

  toListIx :: Array r ix e -> [ListItem ix e]


--unfoldrM :: Monad m => (t -> m (Maybe (a, t))) -> t -> m [a]
unfoldrM :: (Monad m, Ragged r ix e) =>
            (t -> m (Maybe (Elt r ix e, t))) -> t -> m (Array r ix e)
unfoldrM f i = go empty i
  where
    go acc x = do
      res <- f x
      case res of
        Nothing -> return acc
        Just (y, x') -> go (cons y acc) x'

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
  {-# INLINE nestedSz #-}
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
  {-# INLINE unsafeGenerateM #-}
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
  cons x arr = arr {lData = newData, lSize = nestedSz newData}
    where
      newData = cons (lData x) (lData arr)
  {-# INLINE cons #-}
  uncons arr@LArray {..} =
    case uncons lData of
      Nothing -> Nothing
      Just (x, newData) ->
        Just
          ( LArray lComp (nestedSz x) x
          , arr {lData = newData, lSize = nestedSz newData})
  {-# INLINE uncons #-}
  unsafeGenerateM = unsafeGenerateLArrayM
  {-# INLINE unsafeGenerateM #-}
  loadNested using uWrite start end szL arr =
    loadNested using uWrite start end szL (lData arr)
  {-# INLINE loadNested #-}
  raggedFormat f sep arr = raggedFormat f sep (lData arr)
  {-# INLINE raggedFormat #-}


unsafeGenerateLArrayM :: (Monad m, Ragged LN ix e) =>
                         ix -> (ix -> m e) -> m (Array L ix e)
unsafeGenerateLArrayM k f = do
  genData <- unsafeGenerateM k f
  return $ LArray Seq (nestedSz genData) genData
{-# INLINE unsafeGenerateLArrayM #-}


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
  uncons (List [])     = Nothing
  uncons (List (x:xs)) = Just (x, List xs)
  {-# INLINE uncons #-}
  unsafeGenerateM k f =
    loopM (k - 1) (>= 0) (subtract 1) empty $ \i acc -> do
      e <- f i
      return $ cons e acc
  {-# INLINE unsafeGenerateM #-}
  loadNested using uWrite start end _ xs =
    using $ do
      leftOver <-
        loopM start (< end) (+ 1) xs $ \i xs' ->
          case uncons xs' of
            Nothing      -> error $ "Row is too short"
            Just (y, ys) -> uWrite i y >> return ys
      unless (isNull leftOver) $ error "Row is too long"
      return ()
  {-# INLINE loadNested #-}
  fromListIx = coerce
  {-# INLINE fromListIx #-}
  toListIx = coerce
  {-# INLINE toListIx #-}
  raggedFormat f _ (List xs) = L.concat $ "[ " : (L.intersperse "," $ map f xs) ++ [" ]"]


instance ( Index ix
         , Index (Lower ix)
         , Ragged LN (Lower ix) e
         , Elt LN ix e ~ Array LN (Lower ix) e
         , ListItem ix e ~ [ListItem (Lower ix) e]
         ) =>
         Ragged LN ix e where
  isNull (List xs) = null xs
  {-# INLINE isNull #-}
  empty = List []
  {-# INLINE empty #-}
  nestedSz xs =
    consDim (nestedLength xs) $
    case uncons xs of
      Nothing     -> zeroIndex
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
  fromListIx xs = List $ map fromListIx xs
  {-# INLINE fromListIx #-}
  toListIx (List xs) = map toListIx xs
  {-# INLINE toListIx #-}
  raggedFormat f sep (List xs) = showN (raggedFormat f) sep xs


instance (Ragged LN ix e, IsList (Array LN ix e)) => IsList (Array L ix e) where
  type Item (Array L ix e) = Item (Array LN ix e)
  fromList !xs = LArray Seq (nestedSz ls) ls
    where
      !ls = fromList xs :: Array LN ix e
  {-# INLINE fromList #-}
  toList = toList . lData
  {-# INLINE toList #-}


instance IsList (Array LN ix e) where
  type Item (Array LN ix e) = Elt LN ix e
  fromList = coerce
  {-# INLINE fromList #-}
  toList = coerce
  {-# INLINE toList #-}


loadNestedRec :: (Index ix1, Monad m, Ragged r ix e) =>
                 (Int -> Int -> Lower ix1 -> Elt r ix e -> m a)
              -> Int -> Int -> ix1 -> Array r ix e -> m ()
loadNestedRec loadLower start end sz xs = do
  let step = totalElem sz
      szL = snd (unconsDim sz)
  leftOver <-
    loopM start (< end) (+ step) xs $ \ i zs ->
      case uncons zs of
        Nothing -> error "Too short"
        Just (y, ys) -> do
          _ <- loadLower i (i + step) szL y
          return ys
  unless (isNull leftOver) $ error "Too long"
  return ()
{-# INLINE loadNestedRec #-}

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


