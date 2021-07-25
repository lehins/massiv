{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Scheduler
import Control.Monad.ST
import Criterion.Main
import Data.Bifunctor
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench.Common as A
--import qualified Data.Vector.Primitive as VP
import Control.DeepSeq
import Control.Exception
import Control.Monad as M
import Data.Foldable as F
import Data.List as L
import Prelude as P
import Data.Proxy

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
  arrays :: [Matrix P Int] <- evaluate . force =<< M.replicateM 5 (makeRandomArray sz)
  vectors :: [Vector P Int] <- evaluate $ force $ P.map (resize' (Sz (totalElem sz))) arrays
  defaultMain
    [ bgroup
        "Concat"
        [ bench "concatM" $ whnfIO (computeAs P <$> concatM 2 arrays)
        , bench "concatMutableM" $
          whnfIO (concatMutableM arrays :: IO (Matrix P Int))
        , bench "concatMutableM DL" $
          whnfIO (concatMutableM (P.map toLoadArray arrays) :: IO (Matrix P Int))
        , bench "concatOuterM" $
          whnfIO (computeAs P <$> concatOuterM (P.map toLoadArray arrays))
        , bench "concatNewM" $ whnfIO $ concatNewM arrays
        , bench "mconcat (DL)" $ whnf (A.computeAs P . mconcat . P.map toLoadArray) vectors
        ]
    ]

concatMutableM ::
     forall r' r ix e . (Size r', Load r' ix e, Load r ix e, Manifest r e)
  => [Array r' ix e]
  -> IO (Array r ix e)
concatMutableM arrsF =
  case L.uncons arrsF of
    Nothing -> pure empty
    Just (a, arrs) -> do
      let sz = unSz (size a)
          szs = unSz . size <$> arrs
          n = dimensions (Proxy :: Proxy ix)
      (k, szl) <- pullOutDimM sz n
      -- / remove the dimension out of all sizes along which concatenation will happen
      (ks, szls) <-
        F.foldrM (\ !csz (ks, szls) -> bimap (: ks) (: szls) <$> pullOutDimM csz n) ([], []) szs
      -- / make sure to fail as soon as at least one of the arrays has a mismatching inner size
      F.traverse_
        (\(sz', _) -> throwM (SizeMismatchException (SafeSz sz) (SafeSz sz')))
        (P.dropWhile ((== szl) . snd) $ P.zip szs szls)
      let kTotal = SafeSz $ F.foldl' (+) k ks
      newSz <- insertSzM (SafeSz szl) n kTotal
      unsafeCreateArray_ (foldMap getComp arrsF) newSz $ \scheduler marr -> do
        let arrayLoader !offset arr = do
              scheduleWork scheduler $ do
                stToIO $ iterArrayLinearST scheduler arr (\i -> unsafeLinearWrite marr (i + offset))
              pure (offset + totalElem (size arr))
        foldM_ arrayLoader 0 $ a : arrs
{-# INLINE concatMutableM #-}

concatNewM ::
     forall ix e r. (Index ix, Manifest r e, Load r ix e)
  => [Array r ix e]
  -> IO (Array r ix e)
concatNewM arrsF =
  case L.uncons (F.toList arrsF) of
    Nothing -> pure empty
    Just (a, arrs) -> do
      let sz = unSz (size a)
          szs = unSz . size <$> arrs
          n = dimensions (Proxy :: Proxy ix)
      (k, szl) <- pullOutDimM sz n
      -- / remove the dimension out of all sizes along which concatenation will happen
      (ks, szls) <-
        F.foldrM (\ !csz (ks, szls) -> bimap (: ks) (: szls) <$> pullOutDimM csz n) ([], []) szs
      -- / make sure to fail as soon as at least one of the arrays has a mismatching inner size
      F.traverse_
        (\(sz', _) -> throwM (SizeMismatchException (SafeSz sz) (SafeSz sz')))
        (P.dropWhile ((== szl) . snd) $ P.zip szs szls)
      let kTotal = SafeSz $ F.foldl' (+) k ks
      newSz <- insertSzM (SafeSz szl) n kTotal
      unsafeCreateArray_ (foldMap getComp arrsF) newSz $ \scheduler marr -> do
        let arrayLoader !kAcc (kCur, arr) = stToIO $ do
              scheduleWork scheduler $
                iforM_ arr $ \ix e ->
                  let i = getDim' ix n
                      ix' = setDim' ix n (i + kAcc)
                   in unsafeLinearWrite marr (toLinearIndex newSz ix') e
              pure (kAcc + kCur)
        M.foldM_ arrayLoader 0 $ (k, a) : P.zip ks arrs
{-# INLINE concatNewM #-}
