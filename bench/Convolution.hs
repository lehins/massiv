{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Compute
import           Control.Monad
import           Criterion.Main
import           Data.Array.Repa                     as R
import           Data.Array.Repa.Algorithms.Convolve as R
import           Data.Array.Repa.Eval                as R
import           Data.Array.Repa.Repr.Unboxed        as R
import           Data.Array.Repa.Stencil             as R
import           Data.Array.Repa.Stencil.Dim2        as R
import qualified VectorConvolve                      as VC

import           Data.Array.Massiv                   as M
import           Data.Array.Massiv.Convolution       as M

-- import           Data.Array.Massiv.Windowed
-- import           Data.Array.Massiv.Manifest.Unboxed  as M
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


-- | Repa stencil base Kirsch W horizontal convolution
kirschWR
  :: (R.Source r e, Num e) => R.Array r R.DIM2 e
     -> R.Array PC5 R.DIM2 e
kirschWR = mapStencil2 BoundClamp stencil
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :. -1 :. -1 -> Just 5
                      Z :. -1 :.  0 -> Just (-3)
                      Z :. -1 :.  1 -> Just (-3)
                      Z :.  0 :. -1 -> Just 5
                      Z :.  0 :.  1 -> Just (-3)
                      Z :.  1 :. -1 -> Just 5
                      Z :.  1 :.  0 -> Just (-3)
                      Z :.  1 :.  1 -> Just (-3)
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

validate :: (Int, Int) -> (R.Array U R.DIM2 Int, R.Array U R.DIM2 Int)
validate (m, n) = (sR, sVR)
  where
    arrCR = R.computeUnboxedS (arrRLight (m, n) :: R.Array R.D R.DIM2 Int)
    sR = R.computeUnboxedS . sobelGxR $ arrCR
    arrVU = VC.makeVUArray (m, n) lightF :: VC.VUArray Int
    (VC.VUArray _ sV) = VC.applyFilter (VC.sobelFilter VC.Horizontal VC.Edge) arrVU
    sVR = R.fromUnboxed (Z :. m :. n) sV



main :: IO ()
main = do
  let !sz = (1502, 602)
      !arrCR = R.computeUnboxedS (arrR sz)
      !_arrCM = M.computeUnboxedS (arrM sz)
      -- !sobelH = sobelStencil Horizontal
      -- !sobelH' = sobelStencil' Horizontal
      -- !kirschW = kirschWStencil
      -- !kirschW' = kirschWStencil'
      !_sobelHUnroll = makeSobelStencil2D 0
      !sobelHVC = VC.sobelFilter VC.Horizontal VC.Edge
      -- !kirschWUnroll = makeKirschWStencil2D 0
      arrVU :: VC.VUArray Double
      !arrVU = VC.makeVUArray sz lightF
  -- arrCM <- M.computeUnboxedIO (arrM sz)
  --let sobelRAlg = sobelGxRAlg arrCR
  defaultMain
    [ bgroup
        "Sobel Horizontal"
        [ --bench "Massiv check if zero" $ whnf (M.computeUnboxedS . mapStencil2D sobelH) arrCM
        -- , bench "Massiv 1D array with indecies"
        --   $ whnf (M.computeUnboxedS . mapStencil2D sobelH') arrCM
         -- bench "Massiv Unroll" $ whnf (M.computeUnboxedS . mapStencil2D sobelHUnroll) arrCM
         bench "VectorConvolve" $ whnf (VC.applyFilter sobelHVC) arrVU
        , bench "Repa Sobel" $ whnf (R.computeUnboxedS . sobelGxR) arrCR
        --, bench "repa R Agorithms" $ whnfIO sobelRAlg
        ]
        -- "KirschW Horizontal"
        -- [ bench "Massiv check if zero" $ whnf (M.computeUnboxedS . mapStencil2D kirschW) arrCM
        -- -- , bench "Massiv 1D array with indecies"
        -- --   $ whnf (M.computeUnboxedS . mapStencil2D kirschW') arrCM
        -- , bench "Massiv Unroll" $ whnf (M.computeUnboxedS . mapStencil2D kirschWUnroll) arrCM
        -- , bench "Repa KirschW" $ whnf (R.computeUnboxedS . kirschWR) arrCR
        -- --, bench "repa R Agorithms" $ whnfIO kirschWRAlg
        -- ]
    ]
