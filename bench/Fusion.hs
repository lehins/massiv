{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Criterion.Main
import           Prelude               as P

import qualified Data.Array.Massiv     as A
import qualified Data.Vector.Primitive as VP


-- | Very efficient loop
loopI :: Int -> Int -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loopI !start !end increment !initAcc f = go start initAcc where
  go !step !acc =
    case step < end of
      True  -> go (increment step) (f step acc)
      False -> acc
{-# INLINE loopI #-}


-- | Very efficient loop
loop2 :: t -> t -> (t -> Bool) -> (t -> Bool) -> (t -> t) -> a -> (t -> t -> a -> a) -> a
loop2 !init1 !init2 condition1 condition2 increment !initAcc f =
  go init1 init2 initAcc
  where
    go !step1 !step2 !acc
      | condition2 step2 =
        if condition1 step1
          then go (increment step1) step2 (f step1 step2 acc)
          else go init1 (increment step2) acc
      | otherwise = acc
{-# INLINE loop2 #-}

-- -- | Very efficient 2D loop
-- loop2D :: Int -- ^ m rows
--        -> Int -- ^ n columns
--        -> a -- ^ Accumulator
--        -> ((Int, Int) -> a -> a) -- ^ Step function
--        -> a
-- loop2D (I# m) (I# n) !initAcc f = go 0# 0# initAcc
--   where
--     go i j !acc =
--       case j <# n of
--         1# ->
--           case i <# m of
--             1# -> go (1# +# i) j (f (I# i, I# j) acc)
--             _ -> go 0# (1# +# j) acc
--         _ -> acc
-- {-# INLINE loop2D #-}


-- -- | Very efficient loop
-- loop2D :: Int -> Int -> a -> (Int -> Int -> a -> a) -> a
-- loop2D !m !n !initAcc f =
--   go 0 0 initAcc
--   where
--     go !i !j !acc
--       | j < n =
--         if i < m
--           then go (1 + i) j (f i j acc)
--           else go 0 (1 + j) acc
--       | otherwise = acc
-- {-# INLINE loop2D #-}

-- | Very efficient loop
loop2D :: Int -> Int -> a -> ((Int, Int) -> a -> a) -> a
loop2D !m !n !initAcc f =
  go 0 0 initAcc
  where
    go !i !j !acc
      | j < n =
        if i < m
          then go (1 + i) j (f (i, j) acc)
          else go 0 (1 + j) acc
      | otherwise = acc
{-# INLINE loop2D #-}




data Array a = Array Int (Int -> a)

rangea :: Int -> Array Int
rangea n = Array n id

mapa :: (a -> b) -> Array a -> Array b
mapa f (Array size g) = Array size (f . g)

foldra :: (a -> b -> b) -> b -> Array a -> b
foldra f !b (Array size g) = go 0 b
  where
    go !n !b' | n < size  = go (n + 1) (f (g n) b')
              | otherwise = b'

fuseda :: Int -> Int
fuseda = foldra (+) 0 . rangea


data Array2 a = Array2 !Int !Int ((Int, Int) -> a)

rangea2 :: (Int, Int) -> Array2 Int
rangea2 !(m, n) = Array2 m n snd
{-# INLINE rangea2 #-}

-- mapa2 :: (a -> b) -> Array2 a -> Array2 b
-- mapa2 f (Array2 m n g) = Array2 m n (f . g)

foldra2 :: (a -> b -> b) -> b -> Array2 a -> b
foldra2 f !b (Array2 m n g) =
  --loop2D m n b $ \ !i !j !accI -> f (g (i, j)) accI
  loop2D m n b $ \ ix accI -> f (g ix) accI
{-# INLINE foldra2 #-}

-- foldra2 :: (a -> b -> b) -> b -> Array2 a -> b
-- foldra2 f !b (Array2 m n g) =
--   loop2 0 0 (<m) (<n) (+1) b $ \ !i !j !accI -> f (g (i, j)) accI

fuseda2 :: (Int, Int) -> Int
fuseda2 = foldra2 (+) 0 . rangea2


foldra2' :: (a -> b -> b) -> b -> Array2 a -> b
foldra2' f !b (Array2 m n g) = go 0 0 b
  where
    go !i !j !b'
      | j < n =
        if i < m
          then go (i + 1) j (f (g (i, j)) b')
          else go 0 (j + 1) b'
      | otherwise = b'

fuseda2' :: (Int, Int) -> Int
fuseda2' = foldra2' (+) 0 . rangea2


main :: IO ()
main = do
  let !n = 3200000 :: Int
  let !vp = (`VP.generate` id)
  -- let !lsInt = [1 .. n] :: [Int]
  -- let !vecInt = V.fromList lsInt
  -- let !vecSInt = VS.fromList lsInt
  -- let !vecBInt = B.fromList lsInt
  -- let !vecPInt = VP.fromList lsInt
  -- let !vecFInt = vf n
  let vecVInt sz = A.makeArray2D sz snd
  --let vpD n = A.makeArray2D (1, n) (\ !(_, j) -> VP.unsafeIndex (vp n) j)
  defaultMain
    [ -- bgroup
    --     "Sum [Int]"
    --     [ bench "foldl'" $ whnf (F.foldl' (+) 0) lsInt
    --     , bench "foldr'" $ whnf (F.foldr' (+) 0) lsInt
    --     ]
    -- , bgroup
    --     "Sum U (Vector Int)"
    --     [ bench "foldl'" $ whnf (V.foldl' (+) 0) vecInt
    --     , bench "foldr'" $ whnf (V.foldr' (+) 0) vecInt
    --     ]
    -- , bgroup
    --     "Sum S (Vector Int)"
    --     [ bench "foldl'" $ whnf (VS.foldl' (+) 0) vecSInt
    --     , bench "foldr'" $ whnf (VS.foldr' (+) 0) vecSInt
    --     ]
    -- , bgroup
    --     "Sum P (Vector Int)"
    --     [ bench "foldl'" $ whnf (VP.foldl' (+) 0) vecPInt
    --     , bench "foldr'" $ whnf (VP.foldr' (+) 0) vecPInt
    --     ]
    -- , bgroup
    --     "Sum B (Vector Int)"
    --     [ bench "foldl'" $ whnf (B.foldl' (+) 0) vecBInt
    --     , bench "foldr'" $ whnf (B.foldr' (+) 0) vecBInt
    --     ]
    -- ,
      bgroup
        "Sum"
        [ bench "Vector.Array2D Text" $ whnf fuseda2 (2, n)
        , bench "Vector.Array Current" $ whnf (sum . vecVInt) (2, n)
        , bench "Vector.Primitive" $ whnf (VP.sum . vp) n
        , bench "Vector.Array2D Text" $ whnf fuseda2' (1, n)
        , bench "Vector.Array Text" $ whnf fuseda n
        -- , bench "Vector.Fusion" $ whnf (sum . vf) n
        ]
    -- , bgroup
    --     "Sum F (Vector F Int)"
    --     [ bench "F.foldl'" $ whnf (F.foldl' (+) 0) vecFInt
    --     , bench "VF.foldl'" $ whnf (VF.foldl' (+) 0) vecFInt
    --     ]
    -- , bgroup
    --     "Sum . Succ"
    --     [ bench "Vector.Primitive" $ whnf (VP.sum . VP.map succ . vp) n
    --     , bench "Vector.Fusion" $ whnf (sum . fmap succ . vf) n
    --     ]
    -- , bgroup
    --     "Sum . Filter"
    --     [ bench "Vector.Primitive" $ whnf (VP.sum . VP.filter pred' . vp) n
    --     --, bench "Vector.Fusion" $ whnf (sum . VF.filter pred' . vf) n
    --     , bench "Vector.Fusion" $ whnf (sum . P.filter pred' . lf) n
    --     ]
    ]
