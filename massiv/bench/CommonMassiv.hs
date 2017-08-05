{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CommonMassiv where

import           Data.Array.Massiv                  as M hiding ((:.), Z)
import           Data.Array.Massiv.Delayed.Windowed as M
import qualified Data.Vector.Unboxed                as VU



lightF :: Num b => (Int, Int) -> b
lightF !(i, j) =
  fromIntegral
    (round (sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Float)) :: Int)
{-# INLINE lightF #-}

heavyF :: (Floating a2, Integral a) => (a, a) -> a2
heavyF !(i, j) =
        sin (sqrt (sqrt ((fromIntegral i) ** 2 + (fromIntegral j) ** 2)))
{-# INLINE heavyF #-}

vecULight :: (VU.Unbox a, Num a) => (Int, Int) -> VU.Vector a
vecULight !(m, n) = VU.generate (m * n) $ \ !k -> lightF (k `quotRem` n)
{-# INLINE vecULight #-}

vecU :: (Int, Int) -> VU.Vector Double
vecU = vecULight
{-# INLINE vecU #-}

vecU' :: (Int, Int) -> VU.Vector Double
vecU' !(m, n) = VU.generate (m * n) $ \ !k -> heavyF (k `quotRem` n)
{-# INLINE vecU' #-}



arrMLight :: (Num a) => Comp -> (Int, Int) -> Array D DIM2 a
arrMLight comp !arrSz = makeArray comp arrSz lightF
{-# INLINE arrMLight #-}

arrM :: Comp -> (Int, Int) -> Array D DIM2 Double
arrM = arrMLight
{-# INLINE arrM #-}

arrM' :: Comp -> (Int, Int) -> Array D DIM2 Double
arrM' comp !arrSz = makeArray comp arrSz heavyF
{-# INLINE arrM' #-}

arrWindowedM :: (Int, Int) -> Array WD DIM2 Double
arrWindowedM !arrSz@(m, n) =
  makeArrayWindowed (arrM' Par arrSz) (20, 25) (m - 20, n - 25) lightF
{-# INLINE arrWindowedM #-}
