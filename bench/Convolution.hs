{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Control.Monad
import           Criterion.Main
import           Data.Array.Repa                     as R
import           Data.Array.Repa.Algorithms.Convolve as R
import           Data.Array.Repa.Eval                as R
import           Data.Array.Repa.Repr.Unboxed        as R
import           Data.Array.Repa.Stencil             as R
import           Data.Array.Repa.Stencil.Dim2        as R

import           Data.Array.Massiv as M
import           Data.Array.Massiv.Convolution
-- import           Data.Array.Massiv.Windowed
import           Data.Array.Massiv.Manifest.Unboxed  as M
import           Prelude                             as P


-- data Filter r e = Filter
--   { applyFilter :: M.Array r M.DIM2 e -> M.Array W M.DIM2 e -- ^ Apply a filter to an image
--   }


-- sobelFilter :: (Unbox e, Eq e, Num e) => Orientation -> Filter M e
-- sobelFilter dir =
--   Filter (correlate kernel)
--   where
--     !kernel =
--       case dir of
--         Vertical   -> fromListsUnboxed $ [ [ -1, -2, -1 ]
--                                          , [  0,  0,  0 ]
--                                          , [  1,  2,  1 ] ]
--         Horizontal -> fromListsUnboxed $ [ [ -1, 0, 1 ]
--                                          , [ -2, 0, 1 ]
--                                          , [ -1, 0, 1 ] ]
-- {-# INLINE sobelFilter #-}


-- | Repa algorithms convolution implementation
sobelGxRAlg :: (Unbox e, Num e, Monad m) => R.Array U R.DIM2 e -> m (R.Array U R.DIM2 e)
sobelGxRAlg =
  convolveOutP outClamp (R.fromListUnboxed (Z :. 3 :. 3) [-1, 0, 1, -2, 0, 2, -1, 0, 1])

-- | Repa algorithms convolution implementation - separable
sobelGxRAlgSep :: (Unbox e, Num e, Monad m) => R.Array U R.DIM2 e -> m (R.Array U R.DIM2 e)
sobelGxRAlgSep =
  convolveOutP
    outClamp
    (R.fromListUnboxed (Z :. 1 :. 3) [1, 0, -1]) >=>
  convolveOutP
    outClamp
    (R.fromListUnboxed (Z :. 3 :. 1) [1, 2, 1])


-- |  Repa stencil base convolution
sobelGxR
  :: (R.Source r e, Num e) => R.Array r R.DIM2 e
     -> R.Array PC5 R.DIM2 e
sobelGxR = mapStencil2 BoundClamp stencil
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :. -1 :. -1 -> Just (-1)
                      Z :.  0 :. -1 -> Just (-2)
                      Z :.  1 :. -1 -> Just (-1)
                      Z :. -1 :.  1 -> Just 1
                      Z :.  0 :.  1 -> Just 2
                      Z :.  1 :.  1 -> Just 1
                      _             -> Nothing)

forceP
  :: (Load r1 sh e, Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array U sh e)
forceP !arr = do
    forcedArr <- R.computeUnboxedP arr
    forcedArr `deepSeqArray` return forcedArr

forceS
  :: (Load r1 sh e, Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array U sh e)
forceS !arr = do
    forcedArr <- return $ computeS arr
    forcedArr `deepSeqArray` return forcedArr



main :: IO ()
main = do
  let arrR :: R.Array U R.DIM2 Double
      !arrR =
        R.computeUnboxedS $
        fromFunction
          (Z :. (600 :: Int) :. (600 :: Int))
          (\(Z :. i :. j) -> fromIntegral ((min i j) `div` (1 + max i j)))
  let arrM :: M.Array M M.DIM2 Double
      !arrM =
        M.computeUnboxedS $
        makeArray2D
          (600, 600)
          (\ (i, j) -> fromIntegral ((min i j) `div` (1 + max i j)))
  let sobelR = sobelGxR arrR
  --let sobelRAlg = sobelGxRAlg arrR
  --let sobelRAlgSep = sobelGxRAlgSep arrR
  defaultMain
    [ bgroup
        "Sobel"
        [ bench "Repa Stencil Convolution" $ whnfIO (forceS sobelR)
        , bench "Massiv Filter" $ whnf (M.computeUnboxedS . sobelHorizontal) arrM
        --, bench "repa R Agorithms" $ whnfIO sobelRAlg
        ]
    ]
