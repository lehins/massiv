{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module VectorConvolve where

import GHC.Base (quotRemInt)
import Data.Maybe (listToMaybe)
import Control.Monad.ST
import qualified  Data.Vector.Unboxed as VU
import qualified  Data.Vector.Unboxed.Mutable as MVU
import Data.Array.Massiv.Common

data VUArray e = VUArray {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(VU.Vector e)

getVec :: VUArray t -> VU.Vector t
getVec (VUArray _ _ v) = v

instance Show (VUArray e) where

  show (VUArray m n _) = "<VUArray " ++ show m  ++ "x" ++ show n ++ ">"

makeVUArray :: VU.Unbox p =>
               (Int, Int) -> ((Int, Int) -> p) -> VUArray p
makeVUArray !(m, n) f =
  VUArray m n $ VU.generate (m * n) (f . toIx n)
{-# INLINE makeVUArray #-}


makeWindowedVUArray
  :: forall p . VU.Unbox p
  => (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> ((Int, Int) -> p)
  -> ((Int, Int) -> p)
  -> VUArray p
makeWindowedVUArray !(m, n) !(it, jt) !(wm, wn) getWindowPx getBorderPx =
  VUArray m n $ VU.create generate
  where
    !(ib, jb) = (wm + it, wn + jt)
    generate :: ST s (VU.MVector s p)
    generate = do
      mv <- MVU.unsafeNew (m * n)
      loopM_ 0 (< n) (+ 1) $ \ !j -> do
        loopM_ 0 (< it) (+ 1) $ \ !i -> do
          let ix = (i, j)
          MVU.write mv (fromIx n ix) (getBorderPx ix)
        loopM_ ib (< m) (+ 1) $ \ !i -> do
          let ix = (i, j)
          MVU.write mv (fromIx n ix) (getBorderPx ix)
      loopM_ it (< ib) (+ 1) $ \ !i -> do
        loopM_ 0 (< jt) (+ 1) $ \ !j -> do
          let ix = (i, j)
          MVU.write mv (fromIx n ix) (getBorderPx ix)
        loopM_ jb (< n) (+ 1) $ \ !j -> do
          let ix = (i, j)
          MVU.write mv (fromIx n ix) (getBorderPx ix)
      -- loopM_ it (< ib) (+ 1) $ \ i -> do
      --   loopM_ jt (< jb) (+ 1) $ \ j -> do
      --     let ix0 = (i, j)
      --     MVU.write mv (fromIx n ix0) (getWindowPx ix0)
      let !ibS = ib - ((ib - it) `mod` 3)
      loopM_ it (< ibS) (+ 3) $ \ !i -> do
        loopM_ jt (< jb) (+ 1) $ \ !j -> do
          let !ix0 = (i, j)
          let !ix1 = (i + 1, j)
          let !ix2 = (i + 2, j)
          -- let !ix3 = (i + 3, j)
      --     let !ix4 = (i + 4, j)
      --     let !ix5 = (i + 5, j)
      --     let !ix6 = (i + 6, j)
      --     let !ix7 = (i + 7, j)
          MVU.write mv (fromIx n ix0) (getWindowPx ix0)
          MVU.write mv (fromIx n ix1) (getWindowPx ix1)
          MVU.write mv (fromIx n ix2) (getWindowPx ix2)
          -- MVU.write mv (fromIx n ix3) (getWindowPx ix3)
      --     MVU.write mv (fromIx n ix4) (getWindowPx ix4)
      --     MVU.write mv (fromIx n ix5) (getWindowPx ix5)
      --     MVU.write mv (fromIx n ix6) (getWindowPx ix6)
      --     MVU.write mv (fromIx n ix7) (getWindowPx ix7)
      --     --MVU.write mv (fromIx n (i, j)) (getWindowPx (i, j))
      loopM_ ibS (< ib) (+ 1) $ \ !i -> do
        loopM_ jt (< jb) (+ 1) $ \ !j -> do
          MVU.write mv (fromIx n (i, j)) (getWindowPx (i, j))
      return mv
    {-# INLINE generate #-}
{-# INLINE makeWindowedVUArray #-}

fromIx :: Int -- ^ @n@ columns
       -> (Int, Int) -- ^ @(i, j)@ row, column index
       -> Int -- ^ Flat vector index
fromIx !n !(i, j) = n * i + j
{-# INLINE fromIx #-}

toIx :: Int -- ^ @n@ columns
     -> Int -- ^ Flat vector index
     -> (Int, Int) -- ^ @(i, j)@ row, column index
toIx !n !k = quotRemInt k n
{-# INLINE toIx #-}

-- Kernel

-- data Kernel e
--   = Kernel2D {-# UNPACK #-} !Int
--              {-# UNPACK #-} !Int
--              !(VU.Vector (Int, Int, e)) deriving Show

-- toKernel :: (Eq e, Num e, VU.Unbox e) => VUArray e -> Kernel e
-- toKernel (VUArray m n v) =
--   let !(m2, n2) = (m `div` 2, n `div` 2)
--   in Kernel2D m2 n2 $
--      VU.filter (\(_, _, x) -> x /= 0) $ VU.imap (addIx m2 n2 n) v
--   where
--     addIx !m2 !n2 !n' !k x =
--       let !(i, j) = toIx n' k
--       in (i - m2, j - n2, x)
--     {-# INLINE addIx #-}
-- {-# INLINE toKernel #-}

data Kernel e
  = Kernel2D {-# UNPACK #-} !Int
             {-# UNPACK #-} !Int
             !(VU.Vector Int)
             !(VU.Vector Int)
             !(VU.Vector e) deriving Show

toKernel :: (Eq e, Num e, VU.Unbox e) => VUArray e -> Kernel e
toKernel (VUArray m n v) = Kernel2D m2 n2 vM vN vX
  where
    (vM, vN, vX) = VU.unzip3 $ VU.filter (\(_, _, x) -> x /= 0) $ VU.imap addIx v
    !(m2, n2) = (m `div` 2, n `div` 2)
    addIx !k !x =
      let !(i, j) = toIx n k
      in (i - m2, j - n2, x)
    {-# INLINE addIx #-}
{-# INLINE toKernel #-}


uIndex :: MVU.Unbox a => VUArray a -> (Int, Int) -> a
uIndex (VUArray _ n v) !ix = VU.unsafeIndex v (fromIx n ix)
{-# INLINE uIndex #-}


data Border e = Fill e | Wrap | Edge | Reflect | Continue

bIndex :: (MVU.Unbox t, Num t) => (Border t) -> VUArray t -> (Int, Int) -> t
bIndex border arr@(VUArray m n _) !(i, j) =
  if north || east || south || west
  then case border of
    Fill px  -> px
    Wrap     -> getPx (i `mod` m, j `mod` n)
    Edge     -> getPx (if north then 0 else if south then m - 1 else i,
                       if west then 0 else if east then n - 1 else j)
    Reflect  -> getPx (if north then (abs i - 1) `mod` m else
                         if south then (-i - 1) `mod` m else i,
                       if west then (abs j - 1) `mod` n else
                         if east then (-j - 1) `mod` n else j)
    Continue -> getPx (if north then abs i `mod` m else
                         if south then (-i - 2) `mod` m else i,
                       if west then abs j `mod` n else
                         if east then (-j - 2) `mod` n else j)
  else getPx (i, j)
  where
    getPx = uIndex arr
    !north = i < 0
    !south = i >= m
    !west  = j < 0
    !east  = j >= n
{-# INLINE bIndex #-}



-- | Correlate an image with a kernel. Border resolution technique is required.
correlate :: (MVU.Unbox t, Num t, Eq t) =>
     (Border t) -> VUArray t -> VUArray t -> VUArray t
correlate b _ !arr@(VUArray m n _) =
  makeWindowedVUArray
    (m, n)
    (kM2, kN2)
    (m - kM2 * 2, n - kN2 * 2)
    (stencil (uIndex arr))
    (stencil (bIndex b arr))
  where
    (kM2, kN2) = (1, 1)
    stencil getVal !(i, j) =
      getVal (i-1, j-1) * (-1) +
      getVal (  i, j-1) * (-2) +
      getVal (i+1, j-1) * (-1) +
      getVal (i-1, j+1) * 1 +
      getVal (  i, j+1) * 2 +
      getVal (i+1, j+1) * 1
    {-# INLINE stencil #-}
{-# INLINE correlate #-}

-- -- | Correlate an image with a kernel. Border resolution technique is required.
-- correlate :: (MVU.Unbox t, Num t, Eq t) =>
--      (Border t) -> VUArray t -> VUArray t -> VUArray t
-- correlate !b !kernelArr !arr@(VUArray m n _) =
--   makeWindowedVUArray
--     (m, n)
--     (kM2, kN2)
--     (m - kM2 * 2, n - kN2 * 2)
--     (stencil (uIndex arr))
--     (stencil (bIndex b arr))
--   where
--     (Kernel2D kM2 kN2 iVec jVec kVec) = toKernel kernelArr
--     !kLen = VU.length kVec
--     stencil getVal !(i, j) =
--       loop 0 (/= kLen) (+ 1) 0 $ \ !k !acc ->
--         let !iDelta = VU.unsafeIndex iVec k
--             !jDelta = VU.unsafeIndex jVec k
--             !x = VU.unsafeIndex kVec k
--             !val = getVal (i + iDelta, j + jDelta)
--         in acc + x * val
--     {-# INLINE stencil #-}
-- {-# INLINE correlate #-}


-- -- | Correlate an image with a kernel. Border resolution technique is required.
-- correlate :: (MVU.Unbox t, Num t, Eq t) =>
--      t -> VUArray t -> VUArray t -> VUArray t
-- correlate !b !(VUArray kM kN kVec) !arr@(VUArray m n _) =
--   makeWindowedVUArray
--     (m, n)
--     (kM2, kN2)
--     (m - kM2 * 2, n - kN2 * 2)
--     (stencil (uIndex arr))
--     (stencil (bIndex b arr))
--   where
--     (kM2, kN2) = (kM `div` 2, kN `div` 2)
--     stencil getVal !(i, j) = {-# SCC "stencil" #-}
--       loop 0 (/= kM) (+ 1) 0 $ \ !iK !acc0 ->
--         loop 0 (/= kN) (+ 1) acc0 $ \ !jK !acc1 ->
--           {-# SCC "VectorStencilLoop" #-}
--           case VU.unsafeIndex kVec (fromIx kN (iK, jK)) of
--             x | x == 0 -> acc1
--             x -> acc1 + x * getVal (i + iK - kM2, j + jK - kN2)
--     {-# INLINE stencil #-}
-- {-# INLINE correlate #-}


data Filter e = Filter
  { applyFilter :: VUArray e -> VUArray e -- ^ Apply a filter to an image
  }

-- | Used to specify direction for some filters.
data Direction
  = Vertical
  | Horizontal


sobelFilter :: (MVU.Unbox e, Num e, Eq e) => Direction -> (Border e) -> Filter e
sobelFilter dir !border =
  Filter (correlate border kernel)
  where
    !kernel =
      case dir of
        Vertical   -> fromLists $ [ [  1,  2,  1 ]
                                  , [  0,  0,  0 ]
                                  , [ -1, -2, -1 ] ]
        Horizontal -> fromLists $ [ [ -1, 0, 1 ]
                                  , [ -2, 0, 2 ]
                                  , [ -1, 0, 1 ] ]
{-# INLINE sobelFilter #-}



fromLists :: VU.Unbox p => [[p]] -> VUArray p
fromLists !ls =
  if all (== n) (fmap length ls)
    then VUArray m n . VU.fromList . concat $ ls
    else error "Inner lists are of different lengths."
  where
    (m, n) = (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromLists #-}

