{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Core.List
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.List
  ( LN
  , L(..)
  , Array(..)
  , toListArray
  , ListItem
  ) where

import           Control.Monad                (unless)
import           Data.Coerce
import           Data.Foldable                (foldr')
import           Data.Functor.Identity
import qualified Data.List                    as L
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Computation
import           Data.Massiv.Core.Index
import           Data.Massiv.Core.Iterator
import           Data.Massiv.Core.Scheduler
import           GHC.Exts
import           System.IO.Unsafe             (unsafePerformIO)


data LN

type instance EltRepr LN ix = LN

type family ListItem ix e :: * where
  ListItem Ix1 e = e
  ListItem ix  e = [ListItem (Lower ix) e]

type instance NestedStruct LN ix e = [ListItem ix e]

newtype instance Array LN ix e = List { unList :: [Elt LN ix e] }


instance {-# OVERLAPPING #-} Nested LN Ix1 e where
  fromNested = coerce
  {-# INLINE fromNested #-}
  toNested = coerce
  {-# INLINE toNested #-}
  -- flatten = id
  -- {-# INLINE flatten #-}

instance ( Elt LN ix e ~ Array LN (Lower ix) e
         , ListItem ix e ~ [ListItem (Lower ix) e]
         , Coercible (Elt LN ix e) (ListItem ix e)
         ) =>
         Nested LN ix e where
  fromNested = coerce
  {-# INLINE fromNested #-}
  toNested = coerce
  {-# INLINE toNested #-}
  -- flatten (List xs) = List (concatMap (unList . flatten) xs)
  -- {-# INLINE flatten #-}


instance Nested LN ix e => IsList (Array LN ix e) where
  type Item (Array LN ix e) = ListItem ix e
  fromList = fromNested
  {-# INLINE fromList #-}
  toList = toNested
  {-# INLINE toList #-}


data L = L
type instance EltRepr L ix = L

type instance NestedStruct L ix e = Array LN ix e

data instance Array L ix e = LArray { lComp :: Comp
                                    , lSize :: ix
                                    , lData :: !(Array LN ix e) }


instance (Ragged L ix e) => Nested L ix e where
  fromNested xs =
    let newArr = LArray Seq (edgeSize newArr) xs
    in newArr
  {-# INLINE fromNested #-}
  toNested = lData
  {-# INLINE toNested #-}
  -- flatten LArray {..} = LArray { lComp = lComp, lSize = length xs, lData = d }
  --   where d@(List xs) = flatten lData
  -- {-# INLINE flatten #-}


instance (Nested LN ix e, Ragged L ix e) => IsList (Array L ix e) where
  type Item (Array L ix e) = ListItem ix e
  fromList xs =
    let newArr = LArray Seq (edgeSize newArr) (fromNested xs)
    in newArr
  {-# INLINE fromList #-}
  toList = toNested . lData
  {-# INLINE toList #-}



instance {-# OVERLAPPING #-} Ragged L Ix1 e where
  isNull = null . unList . lData
  {-# INLINE isNull #-}
  empty comp = LArray comp 0 (List [])
  {-# INLINE empty #-}
  edgeSize = outerLength
  {-# INLINE edgeSize #-}
  outerLength = length . unList . lData
  {-# INLINE outerLength #-}
  cons x arr = newArr
    where
      newArr = arr { lData = coerce (x : (coerce (lData arr))), lSize = edgeSize newArr }
  {-# INLINE cons #-}
  uncons LArray {..} =
    case L.uncons $ coerce lData of
      Nothing      -> Nothing
      Just (x, xs) -> let newArr = LArray lComp (edgeSize newArr) (coerce xs) in Just (x, newArr)
  {-# INLINE uncons #-}
  unsafeGenerateM comp k f = do
    xs <- loopM (k - 1) (>= 0) (subtract 1) [] $ \i acc -> do
      e <- f i
      return (e:acc)
    return $ LArray comp k $ coerce xs
  {-# INLINE unsafeGenerateM #-}
  loadRagged using uWrite start end _ xs =
    using $ do
      leftOver <-
        loopM start (< end) (+ 1) xs $ \i xs' ->
          case uncons xs' of
            Nothing      -> error $ "Row is too short"
            Just (y, ys) -> uWrite i y >> return ys
      unless (isNull leftOver) $ error "Row is too long"
  {-# INLINE loadRagged #-}
  raggedFormat f _ arr = L.concat $ "[ " : (L.intersperse "," $ map f (coerce (lData arr))) ++ [" ]"]


instance ( Index ix
         , Index (Lower ix)
         , Ragged L (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         , Nested L ix e
         , Coercible (Elt LN ix e) [Elt LN (Lower ix) e]
         ) =>
         Ragged L ix e where
  isNull = null . unList . lData
  {-# INLINE isNull #-}
  empty comp = LArray comp zeroIndex (List [])
  {-# INLINE empty #-}
  edgeSize arr =
    consDim (length (unList (lData arr))) $
    case uncons arr of
      Nothing     -> zeroIndex
      Just (x, _) -> edgeSize x
  {-# INLINE edgeSize #-}
  outerLength = length . unList . lData
  {-# INLINE outerLength #-}
  cons (LArray _ _ x) arr = newArr
    where
      newArr =
        arr {lData = coerce (x : coerce (lData arr)), lSize = edgeSize newArr}
  {-# INLINE cons #-}
  uncons LArray {..} =
    case L.uncons (coerce lData) of
      Nothing -> Nothing
      Just (x, xs) ->
        let newArr = LArray lComp (edgeSize newArr) (coerce xs)
            newX = LArray lComp (edgeSize newX) x
        in Just (newX, newArr)
  {-# INLINE uncons #-}
  unsafeGenerateM comp sz f = do
    let (k, szL) = unconsDim sz
    loopM (k - 1) (>= 0) (subtract 1) (empty comp) $ \i acc -> do
      e <- unsafeGenerateM comp szL (\ixL -> f (consDim i ixL))
      return (cons e acc)
  {-# INLINE unsafeGenerateM #-}
  --loadRagged using uWrite = loadRaggedRec 
  loadRagged using uWrite start end sz xs = do
    let step = totalElem sz
        szL = tailDim sz
    leftOver <-
      loopM start (< end) (+ step) xs $ \ i zs ->
        case uncons zs of
          Nothing -> error "Too short"
          Just (y, ys) -> do
            _ <- loadRagged using uWrite i (i + step) szL y
            return ys
    -- unless (isNull (flatten leftOver)) $ error "Too long"
    unless (isNull leftOver) $ error "Too long"
  {-# INLINE loadRagged #-}
  raggedFormat f sep (LArray comp ix xs) =
    showN (\s y -> raggedFormat f s $ LArray comp (tailDim ix) y) sep (coerce xs)


instance {-# OVERLAPPING #-} (Ragged L ix e, Show e) =>
  Show (Array L ix e) where
  show arr = raggedFormat show "\n" arr

instance {-# OVERLAPPING #-} (Ragged L ix e, Nested LN ix e, Show e) =>
  Show (Array LN ix e) where
  show arr = show (fromNested arr :: Array L ix e)



showN :: (String -> a -> String) -> String -> [a] -> String
showN fShow lnPrefix ls =
  L.concat
    (["[ "] ++
     (L.intersperse (lnPrefix ++ ", ") $ map (fShow (lnPrefix ++ "  ")) ls) ++ [lnPrefix, "]"])


-- loadRaggedRec :: (Index ix1, Monad m, Ragged r Ix1 e, Ragged r ix e) =>
--                  (Int -> Int -> Lower ix1 -> Elt r ix e -> m a)
--               -> Int -> Int -> ix1 -> Array r ix e -> m ()
-- loadRaggedRec loadLower start end sz xs = do
--   let step = totalElem sz
--       szL = tailDim sz
--   leftOver <-
--     loopM start (< end) (+ step) xs $ \ i zs ->
--       case uncons zs of
--         Nothing -> error "Too short"
--         Just (y, ys) -> do
--           _ <- loadLower i (i + step) szL y
--           return ys
--   --unless (isNull (flatten leftOver)) $ error "Too long"
--   unless (isNull leftOver) $ error "Too long"
-- {-# INLINE loadRaggedRec #-}


-- instance {-# OVERLAPPING #-} Ragged LN Ix1 e where
--   isNull (List xs) = null xs
--   {-# INLINE isNull #-}
--   empty = List []
--   {-# INLINE empty #-}
--   edgeSize = outerLength
--   {-# INLINE edgeSize #-}
--   outerLength (List xs) = length xs
--   {-# INLINE outerLength #-}
--   cons x (List xs) = List (x : xs)
--   {-# INLINE cons #-}
--   uncons (List [])     = Nothing
--   uncons (List (x:xs)) = Just (x, List xs)
--   {-# INLINE uncons #-}
--   unsafeGenerateM _ k f =
--     loopM (k - 1) (>= 0) (subtract 1) empty $ \i acc -> do
--       e <- f i
--       return $ cons e acc
--   {-# INLINE unsafeGenerateM #-}
--   loadRagged using uWrite start end _ xs =
--     using $ do
--       leftOver <-
--         loopM start (< end) (+ 1) xs $ \i xs' ->
--           case uncons xs' of
--             Nothing      -> error $ "Row is too short"
--             Just (y, ys) -> uWrite i y >> return ys
--       unless (isNull leftOver) $ error "Row is too long"
--       return ()
--   {-# INLINE loadRagged #-}
--   raggedFormat f _ (List xs) = L.concat $ "[ " : (L.intersperse "," $ map f xs) ++ [" ]"]





instance {-# OVERLAPPING #-} Construct L Ix1 e where
  size = lSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr { lComp = c }
  {-# INLINE setComp #-}
  unsafeMakeArray Seq sz f = runIdentity $ unsafeGenerateM Seq sz (return . f)
  unsafeMakeArray (ParOn wss) sz f = LArray (ParOn wss) sz $ List $ unsafePerformIO $ do
    withScheduler' wss $ \scheduler ->
      loopM_ 0 (< sz) (+ 1) (scheduleWork scheduler . return . f)
  {-# INLINE unsafeMakeArray #-}


instance ( Index ix
         , Ragged L ix e
         , Ragged L (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         ) =>
         Construct L ix e where
  size = lSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr {lComp = c}
  {-# INLINE setComp #-}
  unsafeMakeArray comp sz f = unsafeGenerateN comp sz f
  {-# INLINE unsafeMakeArray #-}

instance (Construct L ix e, Ragged L ix e, Index ix) => Load L ix e where
  loadS arr _ unsafeWrite = let sz = edgeSize arr in
    loadRagged id unsafeWrite 0 (totalElem sz) (tailDim sz) arr
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite = let sz = edgeSize arr in
    withScheduler_ wIds $ \scheduler ->
      loadRagged (scheduleWork scheduler) unsafeWrite 0 (totalElem sz) (tailDim sz) arr
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
unsafeGenerateN Seq sz f = runIdentity $ unsafeGenerateM Seq sz (return . f)
unsafeGenerateN c@(ParOn wss) sz f = unsafePerformIO $ do
  let (m, szL) = unconsDim sz
  xs <- withScheduler' wss $ \scheduler -> do
    loopM_ 0 (< m) (+ 1) $ \i -> scheduleWork scheduler $ do
      unsafeGenerateM c szL $ \ix -> return $ f (consDim i ix)
  return $! foldr' cons (empty c) xs
{-# INLINE unsafeGenerateN #-}


-- class Ragged r ix e where

--   empty :: Array r ix e

--   isNull :: Array r ix e -> Bool

--   cons :: Elt r ix e -> Array r ix e -> Array r ix e

--   uncons :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

--   unsafeGenerateM :: Monad m => ix -> (ix -> m e) -> m (Array r ix e)

--   nestedSz :: Array r ix e -> ix

--   nestedLength :: Array r ix e -> Int

--   -- TODO: test property:
--   -- (read $ raggedFormat show "\n" (ls :: Array L (IxN n) Int)) == ls
--   raggedFormat :: (e -> String) -> String -> Array r ix e -> String

--   loadNested ::
--     (Monad m) =>
--     (m () -> m ()) -> (Int -> e -> m a) -> Int -> Int -> Lower ix -> Array r ix e -> m ()


--   fromListIx :: [ListItem ix e] -> Array r ix e

--   toListIx :: Array r ix e -> [ListItem ix e]


-- --unfoldrM :: Monad m => (t -> m (Maybe (a, t))) -> t -> m [a]
-- unfoldrM :: (Monad m, Ragged r ix e) =>
--             (t -> m (Maybe (Elt r ix e, t))) -> t -> m (Array r ix e)
-- unfoldrM f i = go empty i
--   where
--     go acc x = do
--       res <- f x
--       case res of
--         Nothing -> return acc
--         Just (y, x') -> go (cons y acc) x'


-- showN :: (String -> a -> String) -> String -> [a] -> String
-- showN fShow lnPrefix ls =
--   L.concat
--     (["[ "] ++
--      (L.intersperse (lnPrefix ++ ", ") $ map (fShow (lnPrefix ++ "  ")) ls) ++ [lnPrefix, "]"])


-- instance {-# OVERLAPPING #-} Ragged L Ix1 e where
--   isNull = isNull . lData
--   {-# INLINE isNull #-}
--   empty = LArray Seq zeroIndex empty
--   {-# INLINE empty #-}
--   nestedSz = nestedSz . lData
--   {-# INLINE nestedSz #-}
--   nestedLength = nestedLength . lData
--   {-# INLINE nestedLength #-}
--   cons x arr = newArr
--     where
--       newArr = arr {lData = cons x (lData arr), lSize = nestedSz newArr}
--   {-# INLINE cons #-}
--   uncons LArray {..} =
--     case uncons lData of
--       Nothing      -> Nothing
--       Just (x, xs) -> Just (x, LArray lComp (nestedSz xs) xs)
--   {-# INLINE uncons #-}
--   unsafeGenerateM = unsafeGenerateLArrayM
--   {-# INLINE unsafeGenerateM #-}
--   loadNested using uWrite start end szL arr =
--     loadNested using uWrite start end szL (lData arr)
--   raggedFormat f sep arr = raggedFormat f sep (lData arr)

-- instance ( Index ix
--          , Ragged LN ix e
--          , Ragged LN (Lower ix) e
--          , Elt LN ix e ~ Array LN (Lower ix) e
--          , Elt L ix e ~ Array L (Lower ix) e
--          ) =>
--          Ragged L ix e where
--   isNull = isNull . lData
--   {-# INLINE isNull #-}
--   empty = LArray Seq zeroIndex empty
--   {-# INLINE empty #-}
--   nestedSz = nestedSz . lData
--   {-# INLINE nestedSz #-}
--   nestedLength = nestedLength . lData
--   {-# INLINE nestedLength #-}
--   cons x arr = arr {lData = newData, lSize = nestedSz newData}
--     where
--       newData = cons (lData x) (lData arr)
--   {-# INLINE cons #-}
--   uncons arr@LArray {..} =
--     case uncons lData of
--       Nothing -> Nothing
--       Just (x, newData) ->
--         Just
--           ( LArray lComp (nestedSz x) x
--           , arr {lData = newData, lSize = nestedSz newData})
--   {-# INLINE uncons #-}
--   unsafeGenerateM = unsafeGenerateLArrayM
--   {-# INLINE unsafeGenerateM #-}
--   loadNested using uWrite start end szL arr =
--     loadNested using uWrite start end szL (lData arr)
--   {-# INLINE loadNested #-}
--   raggedFormat f sep arr = raggedFormat f sep (lData arr)
--   {-# INLINE raggedFormat #-}


-- unsafeGenerateLArrayM :: (Monad m, Ragged LN ix e) =>
--                          ix -> (ix -> m e) -> m (Array L ix e)
-- unsafeGenerateLArrayM k f = do
--   genData <- unsafeGenerateM k f
--   return $ LArray Seq (nestedSz genData) genData
-- {-# INLINE unsafeGenerateLArrayM #-}



-- instance ( Index ix
--          , Index (Lower ix)
--          , Ragged LN (Lower ix) e
--          , Elt LN ix e ~ Array LN (Lower ix) e
--          , ListItem ix e ~ [ListItem (Lower ix) e]
--          ) =>
--          Ragged LN ix e where
--   isNull (List xs) = null xs
--   {-# INLINE isNull #-}
--   empty = List []
--   {-# INLINE empty #-}
--   nestedSz xs =
--     consDim (nestedLength xs) $
--     case uncons xs of
--       Nothing     -> zeroIndex
--       Just (x, _) -> nestedSz x
--   {-# INLINE nestedSz #-}
--   nestedLength (List xs) = length xs
--   {-# INLINE nestedLength #-}
--   cons x (List xs) = List (x : xs)
--   {-# INLINE cons #-}
--   uncons (List []) = Nothing
--   uncons (List (x:xs)) = Just (x, List xs)
--   {-# INLINE uncons #-}
--   loadNested using uWrite = loadNestedRec (loadNested using uWrite)
--   {-# INLINE loadNested #-}
--   unsafeGenerateM sz f = do
--     let (k, szL) = unconsDim sz
--     loopM (k - 1) (>= 0) (subtract 1) empty $ \i acc -> do
--       e <- unsafeGenerateM szL (\ixL -> f (consDim i ixL))
--       return $ cons e acc
--   {-# INLINE unsafeGenerateM #-}
--   fromListIx xs = List $ map fromListIx xs
--   {-# INLINE fromListIx #-}
--   toListIx (List xs) = map toListIx xs
--   {-# INLINE toListIx #-}
--   raggedFormat f sep (List xs) = showN (raggedFormat f) sep xs


-- instance (Ragged LN ix e, IsList (Array LN ix e)) => IsList (Array L ix e) where
--   type Item (Array L ix e) = Item (Array LN ix e)
--   fromList !xs = LArray Seq (nestedSz ls) ls
--     where
--       !ls = fromList xs :: Array LN ix e
--   {-# INLINE fromList #-}
--   toList = toList . lData
--   {-# INLINE toList #-}


-- instance IsList (Array LN ix e) where
--   type Item (Array LN ix e) = Elt LN ix e
--   fromList = coerce
--   {-# INLINE fromList #-}
--   toList = coerce
--   {-# INLINE toList #-}


-- loadNestedRec :: (Index ix1, Monad m, Ragged r ix e) =>
--                  (Int -> Int -> Lower ix1 -> Elt r ix e -> m a)
--               -> Int -> Int -> ix1 -> Array r ix e -> m ()
-- loadNestedRec loadLower start end sz xs = do
--   let step = totalElem sz
--       szL = snd (unconsDim sz)
--   leftOver <-
--     loopM start (< end) (+ step) xs $ \ i zs ->
--       case uncons zs of
--         Nothing -> error "Too short"
--         Just (y, ys) -> do
--           _ <- loadLower i (i + step) szL y
--           return ys
--   unless (isNull leftOver) $ error "Too long"
--   return ()
-- {-# INLINE loadNestedRec #-}

-- toListArray ::
--      forall r ix e. (IsList (Array L ix e), Construct L ix e, Source r ix e)
--   => Array r ix e
--   -> [Item (Array L ix e)]
-- toListArray arr =
--   toList
--     (unsafeMakeArray (getComp arr) (size arr) (unsafeIndex arr) :: Array L ix e)
-- {-# INLINE toListArray #-}


toListArray :: (Construct L ix e, Source r ix e)
            => Array r ix e
            -> Array L ix e
toListArray arr =
  unsafeMakeArray (getComp arr) (size arr) (unsafeIndex arr)
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



-- ALTRENATIVE
-------------------------------------------------------------------------
-- data L = L
-- type instance EltRepr L ix = L

-- data instance Array L ix e = LArray { lComp :: Comp
--                                     , lSize :: ix
--                                     , lData :: ![NestedItem L ix e] }


-- instance {-# OVERLAPPING #-} Construct L Ix1 e where
--   size = lSize
--   {-# INLINE size #-}
--   getComp = lComp
--   {-# INLINE getComp #-}
--   setComp c arr = arr { lComp = c }
--   {-# INLINE setComp #-}
--   unsafeMakeArray Seq sz f = runIdentity $ unsafeGenerateM Seq sz (return . f)
--   unsafeMakeArray (ParOn wss) sz f = LArray (ParOn wss) sz $ unsafePerformIO $ do
--     withScheduler' wss $ \scheduler ->
--       loopM_ 0 (< sz) (+ 1) (scheduleWork scheduler . return . f)
--   {-# INLINE unsafeMakeArray #-}


-- instance ( Index ix
--          , Index (Lower ix)
--          , Ragged L ix e
--          , Ragged L (Lower ix) e
--          , Elt L ix e ~ Array L (Lower ix) e
--          ) =>
--          Construct L ix e where
--   size = lSize
--   {-# INLINE size #-}
--   getComp = lComp
--   {-# INLINE getComp #-}
--   setComp c arr = arr {lComp = c}
--   {-# INLINE setComp #-}
--   unsafeMakeArray comp sz f = unsafeGenerateN comp sz f
--   {-# INLINE unsafeMakeArray #-}



-- unsafeGenerateN ::
--   ( Index ix
--   , Ragged r ix e
--   , Ragged r (Lower ix) e
--   , Elt r ix e ~ Array r (Lower ix) e )
--   => Comp
--   -> ix
--   -> (ix -> e)
--   -> Array r ix e
-- unsafeGenerateN Seq sz f = runIdentity $ unsafeGenerateM Seq sz (return . f)
-- unsafeGenerateN comp@(ParOn wss) sz f = unsafePerformIO $ do
--   let (m, szL) = unconsDim sz
--   xs <- withScheduler' wss $ \scheduler -> do
--     loopM_ 0 (< m) (+ 1) $ \i -> scheduleWork scheduler $ do
--       unsafeGenerateM comp szL $ \ix -> return $ f (consDim i ix)
--   return $! foldr' cons (empty comp) xs
-- {-# INLINE unsafeGenerateN #-}


-- instance {-# OVERLAPPING #-} Ragged L Ix1 e where
--   type Nested L Ix1 e = [NestedItem L Ix1 e]
--   isNull (LArray _ _ xs) = null xs
--   {-# INLINE isNull #-}
--   empty comp = LArray comp 0 []
--   {-# INLINE empty #-}
--   edgeSize = outerLength
--   {-# INLINE edgeSize #-}
--   outerLength = length . lData
--   {-# INLINE outerLength #-}
--   cons x arr = newArr
--     where
--       newArr = arr { lData = x : lData arr, lSize = edgeSize newArr }
--   {-# INLINE cons #-}
--   uncons LArray {..} =
--     case L.uncons lData of
--       Nothing      -> Nothing
--       Just (x, xs) -> let newArr = LArray lComp (edgeSize newArr) xs in Just (x, newArr)
--   {-# INLINE uncons #-}
--   unsafeGenerateM comp k f = do
--     xs <- loopM (k - 1) (>= 0) (subtract 1) [] $ \i acc -> do
--       e <- f i
--       return (e:acc)
--     return $ LArray comp k xs
--   {-# INLINE unsafeGenerateM #-}
--   loadRagged using uWrite start end _ xs =
--     using $ do
--       leftOver <-
--         loopM start (< end) (+ 1) xs $ \i xs' ->
--           case uncons xs' of
--             Nothing      -> error $ "Row is too short"
--             Just (y, ys) -> uWrite i y >> return ys
--       unless (isNull leftOver) $ error "Row is too long"
--   {-# INLINE loadRagged #-}
--   fromNested comp xs = LArray comp (length xs) xs
--   {-# INLINE fromNested #-}
--   toNested = lData
--   {-# INLINE toNested #-}
--   raggedFormat f _ arr = L.concat $ "[ " : (L.intersperse "," $ map f (lData arr)) ++ [" ]"]


-- instance ( Index ix
--          , Index (Lower ix)
--          , Ragged L (Lower ix) e
--          , Elt L ix e ~ Array L (Lower ix) e
--          , NestedItem L ix e ~ [NestedItem L (Lower ix) e]
--          , [NestedItem L (Lower ix) e] ~ NestedItem L ix [NestedItem L (Lower ix) e]
--          ) =>
--          Ragged L ix e where
--   type Nested L ix e = [NestedItem L ix e]
--   isNull (LArray _ _ xs) = null xs
--   {-# INLINE isNull #-}
--   empty comp = LArray comp zeroIndex []
--   {-# INLINE empty #-}
--   edgeSize arr =
--     consDim (length (lData arr)) $
--     case uncons arr of
--       Nothing -> zeroIndex
--       Just (x, _) -> edgeSize x
--   {-# INLINE edgeSize #-}
--   outerLength = length . lData
--   {-# INLINE outerLength #-}
--   cons (LArray _ _ x) arr = newArr
--     where
--       newArr = arr {lData = x : lData arr, lSize = edgeSize newArr}
--   {-# INLINE cons #-}
--   uncons LArray {..} =
--     case L.uncons lData of
--       Nothing -> Nothing
--       Just (x, xs) ->
--         let newArr = LArray lComp (edgeSize newArr) xs
--             newX = LArray lComp (edgeSize newX) x
--         in Just (newX, newArr)
--   {-# INLINE uncons #-}
--   unsafeGenerateM comp sz f = do
--     let (k, szL) = unconsDim sz
--     loopM (k - 1) (>= 0) (subtract 1) (empty comp) $ \i acc -> do
--       e <- unsafeGenerateM comp szL (\ixL -> f (consDim i ixL))
--       return (cons e acc)
--   {-# INLINE unsafeGenerateM #-}
--   loadRagged using uWrite = loadRaggedRec (loadRagged using uWrite)
--   {-# INLINE loadRagged #-}
--   fromNested comp xs =
--     let newArr = LArray comp (edgeSize newArr) xs in newArr
--   {-# INLINE fromNested #-}
--   toNested = lData
--   {-# INLINE toNested #-}
--   raggedFormat f sep (LArray comp ix xs) =
--     showN (\s y -> raggedFormat f s $ LArray comp (tailDim ix) y) sep xs

-- -- TODO: remove construct
-- instance (Construct L ix e, Ragged L ix e, Index ix) => Load L ix e where
--   loadS arr _ unsafeWrite = let sz = edgeSize arr in
--     loadRagged id unsafeWrite 0 (totalElem sz) (tailDim sz) arr
--   {-# INLINE loadS #-}
--   loadP wIds arr _ unsafeWrite = let sz = edgeSize arr in
--     withScheduler_ wIds $ \scheduler ->
--       loadRagged (scheduleWork scheduler) unsafeWrite 0 (totalElem sz) (tailDim sz) arr
--   {-# INLINE loadP #-}


-- instance Ragged L ix e => IsList (Array L ix e) where
--   type Item (Array L ix e) = NestedItem L ix e
--   fromList xs = let newArr = LArray Seq (edgeSize newArr) (coerce xs) in newArr
--   {-# INLINE fromList #-}
--   toList = lData
--   {-# INLINE toList #-}



