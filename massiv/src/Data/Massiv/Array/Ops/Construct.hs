{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Construct
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Construct
  ( makeVectorR
  , makeArray
  , makeArrayR
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  , fromListIx1
  , fromListIx1As
  , fromListIx2
  , fromListIx2As
  , fromListIx3
  , fromListIx3As
  , toListIx1
  , toListIx2
  , toListIx2'
  , toListIx3
  , toListIx4
  -- Lower level
  , fromListSIx1
  , fromListSIx2
  , fromListPIx2
  , fromListSIx3
  , fromListPIx3
  ) where

import           Control.Monad                      (unless)
import           Control.Monad.ST                   (runST)
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Array.Ops.Slice        ((!>))
import           Data.Massiv.Core.Scheduler
import           Data.Massiv.Core
import           Data.Maybe                         (listToMaybe)
import           GHC.Base                           (build)
import           Prelude                            as P
import           System.IO.Unsafe                   (unsafePerformIO)



-- | Just like `makeArrayR`, but create a flat Array with a specified representation
makeVectorR :: Construct r Int e => r -> Comp -> Int -> (Int -> e) -> Array r Int e
makeVectorR = makeArrayR
{-# INLINE makeVectorR #-}


-- | Create an Array.
makeArray :: Construct r ix e =>
             Comp -- ^ Computation strategy. Useful constructors are `Seq` and `Par`
          -> ix -- ^ Size of the result Array
          -> (ix -> e) -- ^ Function to generate elements for a particular index
          -> Array r ix e
makeArray !c = unsafeMakeArray c . liftIndex (max 0)
{-# INLINE makeArray #-}

-- | Just like `makeArray` but with ability to specify the result representation
makeArrayR :: Construct r ix e => r -> Comp -> ix -> (ix -> e) -> Array r ix e
makeArrayR _ = makeArray
{-# INLINE makeArrayR #-}


-- | Create a vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> toListIx1 $ range Seq 1 6
-- [1,2,3,4,5]
-- >>> toListIx1 $ range Seq (-2) 3
-- [-2,-1,0,1,2]
range :: Comp -> Int -> Int -> Array D Ix1 Int
range comp !from !to = makeArray comp (max 0 (to - from)) (+ from)
{-# INLINE range #-}



rangeStep :: Comp -- ^ Computation strategy
          -> Int -- ^ Start
          -> Int -- ^ Step (Can't be zero)
          -> Int -- ^ End
          -> Array D Ix1 Int
rangeStep comp !from !step !to
  | step == 0 = error "rangeStep: Step can't be zero"
  | otherwise =
    let (sz, r) = (to - from) `divMod` step
    in makeArray comp (sz + signum r) (\i -> from + i * step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toListIx1 $ enumFromN Seq 5 3
-- [5,6,7]
--
enumFromN :: Num e =>
             Comp
          -> e -- ^ Start value
          -> Int -- ^ Length of resulting array
          -> Array D Ix1 e
enumFromN comp !from !sz = makeArray comp sz $ \ i -> fromIntegral i + from
{-# INLINE enumFromN #-}

-- |
--
-- >>> toListIx1 $ enumFromStepN Seq 1 0.1 5
-- [1.0,1.1,1.2,1.3,1.4]
--
enumFromStepN :: Num e =>
                 Comp
              -> e -- ^ Start value
              -> e -- ^ Step value
              -> Int -- ^ Length of resulting vector
              -> Array D Ix1 e
enumFromStepN comp !from !step !sz = makeArray comp sz $ \ i -> from + fromIntegral i * step
{-# INLINE enumFromStepN #-}


fromListSIx1 :: Mutable r Ix1 e => Comp -> Int -> [e] -> Array r Ix1 e
fromListSIx1 comp k xs =
  runST $ do
    mArr <- unsafeNew k
    loadListIx1 id (unsafeLinearWrite mArr) 0 k (Ix1 0) xs
    unsafeFreeze comp mArr
{-# INLINE fromListSIx1 #-}


fromListIx1 :: Mutable r Ix1 e => Comp -> [e] -> Array r Ix1 e
fromListIx1 comp xs = fromListSIx1 comp (P.length xs) xs
{-# INLINE fromListIx1 #-}


-- | fromListIx1As U Par [1,2,3]
fromListIx1As :: Mutable r Ix1 e => r -> Comp -> [e] -> Array r Ix1 e
fromListIx1As _ = fromListIx1
{-# INLINE fromListIx1As #-}


fromListIx2 :: Mutable r Ix2 e => Comp -> [[e]] -> Array r Ix2 e
fromListIx2 Seq          xs = fromListSIx2 (P.length xs) xs
fromListIx2 (ParOn wIds) xs = fromListPIx2 wIds (P.length xs) xs
{-# INLINE fromListIx2 #-}


fromListIx2As :: Mutable r Ix2 e => r -> Comp -> [[e]] -> Array r Ix2 e
fromListIx2As _ = fromListIx2
{-# INLINE fromListIx2As #-}



fromListIx3 :: Mutable r Ix3 e => Comp -> [[[e]]] -> Array r Ix3 e
fromListIx3 Seq          xs = fromListSIx3 (P.length xs) xs
fromListIx3 (ParOn wIds) xs = fromListPIx3 wIds (P.length xs) xs
{-# INLINE fromListIx3 #-}


fromListIx3As :: Mutable r Ix3 e => r -> Comp -> [[[e]]] -> Array r Ix3 e
fromListIx3As _ = fromListIx3
{-# INLINE fromListIx3As #-}

loadListIx1 :: Monad m => (m () -> m ()) -> (Int -> t -> m a) -> Int -> Int -> b -> [t] -> m ()
loadListIx1 using uWrite start end _ xs = using $ do
  leftOver <- loopM start (< end) (+ 1) xs $ \ i xs' ->
    case xs' of
      (y:ys) -> uWrite i y >> return ys
      []     -> error $ "Row is too short"
  unless (P.null leftOver) $ error "Row is too long"
{-# INLINE loadListIx1 #-}

loadListUsingIx2 :: (Index ix, Monad m) =>
  Int -> Int -> ix -> (Int -> e -> m ()) -> (m () -> m ()) -> [[e]] -> m ()
loadListUsingIx2 start end sz uWrite using xs = do
  loadListUsingN (loadListIx1 using uWrite) start end sz xs
{-# INLINE loadListUsingIx2 #-}

loadListUsingIx3 :: (Index ix, Index (Lower ix), Monad m) =>
  Int -> Int -> ix -> (Int -> e -> m ()) -> (m () -> m ()) -> [[[e]]] -> m ()
loadListUsingIx3 start end sz uWrite using xs = do
  loadListUsingN (loadListUsingN (loadListIx1 using uWrite)) start end sz xs
{-# INLINE loadListUsingIx3 #-}

loadListUsingN :: (Index ix, Monad m) =>
                  (Int -> Int -> Lower ix -> e -> m ()) -> Int -> Int -> ix -> [e] -> m ()
loadListUsingN loadLower start end sz xs = do
  let step = totalElem sz
      szL = snd (unconsDim sz)
  leftOver <-
    loopM start (< end) (+ step) xs $ \ !i zs ->
      case zs of
        [] -> error "Too short"
        (y:ys) -> do
          loadLower i (i + step) szL y
          return ys
  unless (null leftOver) $ error "Too long"
{-# INLINE loadListUsingN #-}

-- fromListSIx :: Mutable r Ix3 e => Int -> [[[e]]] -> Array r Ix3 e
-- fromListSIx k xs =
--   runST $ do
--     let mFirstRow = listToMaybe xs
--     let sz@(_ :> szL@(_ :. _)) =
--           (k :> maybe 0 P.length mFirstRow :.
--            maybe 0 P.length (mFirstRow >>= listToMaybe))
--     mArr <- unsafeNew sz
--     loadListUsingN (
--       loadListUsingN (
--           loadListIx1 id (unsafeLinearWrite mArr))) 0 (totalElem sz) szL xs
--     unsafeFreeze Seq mArr
-- {-# INLINE fromListSIx3 #-}

fromListSIx2 :: Mutable r Ix2 e => Int -> [[e]] -> Array r Ix2 e
fromListSIx2 k xs =
  runST $ do
    let sz@(m :. n) = (k :. maybe 0 P.length (listToMaybe xs))
    mArr <- unsafeNew sz
    loadListUsingIx2 0 (m * n) n (unsafeLinearWrite mArr) id xs
    unsafeFreeze Seq mArr
{-# INLINE fromListSIx2 #-}


fromListSIx3 :: Mutable r Ix3 e => Int -> [[[e]]] -> Array r Ix3 e
fromListSIx3 k xs =
  runST $ do
    let mFirstRow = listToMaybe xs
    let sz@(_ :> szL@(_ :. _)) =
          (k :> maybe 0 P.length mFirstRow :.
           maybe 0 P.length (mFirstRow >>= listToMaybe))
    mArr <- unsafeNew sz
    loadListUsingIx3 0 (totalElem sz) szL (unsafeLinearWrite mArr) id xs
    unsafeFreeze Seq mArr
{-# INLINE fromListSIx3 #-}


-- | Convert a list of lists into a 2 dimensional array in parallel, while
-- splitting loading of all rows among all cores/capabilites.
fromListPIx2 :: Mutable r Ix2 e => [Int] -> Int -> [[e]] -> Array r Ix2 e
fromListPIx2 wIds k xs =
  unsafePerformIO $ do
    let sz@(m :. n) = (k :. maybe 0 P.length (listToMaybe xs))
    mArr <- unsafeNew sz
    withScheduler_ wIds $ \ scheduler -> do
      loadListUsingIx2
        0
        (m * n)
        n
        (unsafeLinearWrite mArr)
        (scheduleWork scheduler)
        xs
    unsafeFreeze Par mArr
{-# INLINE fromListPIx2 #-}


-- fromRaggedP :: (Rank ix ~ n, Mutable r ix e) => [Int] -> Int -> rag n (E e) -> Array r ix e
-- fromRaggedP wss k xs =
--   unsafePerformIO $ do
--     let sz = raggedSize xs :: ix
--     mArr <- unsafeNew sz
--     withScheduler_ wIds $ \scheduler -> do
--       loadRaggedUsingIx3
--         0
--         (totalElem sz)
--         (m * n)
--         n
--         (unsafeLinearWrite mArr)
--         (scheduleWork scheduler)
--         xs
--     unsafeFreeze Par mArr

fromListPIx3 :: Mutable r Ix3 e => [Int] -> Int -> [[[e]]] -> Array r Ix3 e
fromListPIx3 wIds k xs =
  unsafePerformIO $ do
    let mFirstRow = listToMaybe xs
    let sz@(l :> m :. n) =
          (k :> maybe 0 P.length mFirstRow :.
           maybe 0 P.length (mFirstRow >>= listToMaybe)) :: Ix3
    mArr <- unsafeNew (l :> m :. n)
    withScheduler_ wIds $ \scheduler -> do
      loadListUsingIx3
        0
        (totalElem sz)
        (tailDim sz)
        (unsafeLinearWrite mArr)
        (scheduleWork scheduler)
        xs
    unsafeFreeze Par mArr
{-# INLINE fromListPIx3 #-}


-- | Convert an array into a list.
--
-- >>> toListIx1 $ makeArrayIx2 (2, 3) id
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
toListIx1 :: Source r ix e => Array r ix e -> [e]
toListIx1 !arr = build (\ c n -> foldrFB c n arr)
{-# INLINE toListIx1 #-}


-- | Convert an array with at least 2 dimensions into a list of lists. Inner
-- dimensions will get flattened into a list.
--
-- ==== __Examples__
--
-- >>> toListIx2 $ makeArrayIx2 Seq (2, 3) id
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toListIx2 $ makeArrayIx3 Seq (2, 1, 3) id
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toListIx2 :: (Source r ix e, Index (Lower ix)) => Array r ix e -> [[e]]
toListIx2 = toListIx1 . foldrInner (:) []
{-# INLINE toListIx2 #-}

-- | Right fold with an index aware function of inner most dimension.
foldrInner :: (Source r ix e, Index (Lower ix)) =>
              (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrInner f !acc !arr =
  unsafeMakeArray (getComp arr) szL $ \ !ix ->
    foldrS f acc $ makeArrayR D Seq m (unsafeIndex arr . snocDim ix)
  where
    !(szL, m) = unsnocDim (size arr)
{-# INLINE foldrInner #-}

toListIx2' :: (Elt r ix e ~ Array (EltRepr r ix) (Lower ix) e, Source (EltRepr r ix) (Lower ix) e, OuterSlice r ix e) => Array r ix e -> [[e]]
toListIx2' !arr = build $ \ c n -> foldrFB c n $ fmap toListIx1 $ makeArrayR D Seq k (arr !>)
  where !k = fst $ unconsDim $ size arr
{-# INLINE toListIx2' #-}


toListIx3 :: (Index (Lower (Lower ix)), Index (Lower ix), Source r ix e) => Array r ix e -> [[[e]]]
toListIx3 = toListIx1 . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toListIx3 #-}

toListIx4 ::
     ( Index (Lower (Lower (Lower ix)))
     , Index (Lower (Lower ix))
     , Index (Lower ix)
     , Source r ix e
     )
  => Array r ix e
  -> [[[[e]]]]
toListIx4 = toListIx1 . foldrInner (:) [] . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toListIx4 #-}


-- toListIx3 :: (Slice (R r) (Lower ix) e, Slice r ix e) => Array r ix e -> [[[e]]]
-- toListIx3 !arr = build $ \ c n -> foldrFB c n $ fmap toListIx2 $ makeArrayR D Seq k (arr !>)
--   where !k = fst $ unconsDim $ size arr
-- {-# INLINE toListIx3 #-}




-- -- | Left fold with an index aware function of inner most dimension.
-- ifoldlInner :: (Source r ix e, Index (Lower ix)) =>
--   (a -> ix -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
-- ifoldlInner f !acc !arr =
--   unsafeMakeArray Par szL $ \ix ->
--     let g a j e = f a (snocDim ix j) e
--     in ifoldlS g acc $ DArray Seq m (\i -> unsafeIndex arr (snocDim ix i))
--   where
--     !(szL, m) = unsnocDim (size arr)
-- {-# INLINE ifoldlInner #-}


-- -- | Right fold with an index aware function of inner most dimension.
-- ifoldrInner :: (Source r ix e, Index (Lower ix)) =>
--   (ix -> e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
-- ifoldrInner f !acc !arr =
--   unsafeMakeArray (getComp arr) szL $ \ix ->
--     let g j e a = f (snocDim ix j) e a
--     in ifoldrS g acc $ DArray Seq m (\i -> unsafeIndex arr (snocDim ix i))
--   where
--     !(szL, m) = unsnocDim (size arr)
-- {-# INLINE ifoldrInner #-}



-- toListPIx2
--   :: forall r ix e . Slice r ix e => Array r ix e -> [[e]]
-- toListPIx2 !arr = unsafePerformIO $ do
--   arrLs <- sequenceP $
--     makeArrayIx1 k (return . toListSIx1 . (arr !>))
--   return $ toListSIx1 (arrLs :: Array B Ix1 [e])
--   where
--     !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx2 #-}


-- toListPIx3
--   :: forall r ix e . (Slice (R r) (Lower ix) e, Slice r ix e) => Array r ix e -> [[[e]]]
-- toListPIx3 !arr = unsafePerformIO $ do
--   arrLs <- sequenceP $
--     makeArrayIx1 k (return . toListSIx2 . (arr !>))
--   return $ toListSIx1 (arrLs :: Array B Ix1 [[e]])
--   where
--     !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx3 #-}


-- toListPIx2 :: (NFData e, Slice r ix e) => Array r ix e -> [[e]]
-- toListPIx2 !arr = unsafePerformIO $ foldrP (++) [] (:) [] $ fmap toListSIx1 $ makeArrayIx1 k (arr !>)
--   where !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx2 #-}

-- toListPIx2' :: (NFData e, Slice r ix e) => Array r ix e -> IO [[e]]
-- toListPIx2' = foldrP' (:) []
-- {-# INLINE toListPIx2' #-}

-- toListPIx2' :: (NFData e, Index (Lower ix), Source r ix e) => Array r ix e -> [[e]]
-- toListPIx2' !arr =
--   concat $ unsafePerformIO $
--   foldrP (:) [] (:) [] $
--   fmap toListSIx1 $
--   makeArrayIx1 m (\ !i -> DArray szL (\ !ix -> unsafeIndex arr (consDim i ix)))
--   where
--     !(m, szL) = unconsDim (size arr)
-- {-# INLINE toListPIx2' #-}


-- toListPIx3 :: (NFData e, Slice (R r) (Lower ix) e, Slice r ix e) =>
--              Array r ix e -> [[[e]]]
-- toListPIx3 !arr = unsafePerformIO $ foldrP (++) [] (:) [] $ fmap toListSIx2 $ makeArrayIx1 k (arr !>)
--   where !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx3 #-}
