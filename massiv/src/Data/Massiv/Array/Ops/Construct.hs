{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Construct
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Construct
  ( makeArray
  , makeArrayR
  , singleton
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  , toListIx1
  , toListIx2
  , toListIx2'
  , toListIx3
  , toListIx4
  ) where

import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Array.Ops.Slice        ((!>))
import           Data.Massiv.Core.Common
import           GHC.Base                           (build)
import           Prelude                            as P


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
