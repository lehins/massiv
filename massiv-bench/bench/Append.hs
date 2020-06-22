{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import qualified Data.DList as DL
import qualified Data.List as List
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Bench as A
import Data.Maybe
import qualified Data.Vector.Primitive as VP
import Prelude as P

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !len = totalElem sz
      !arr = computeAs P $ resize' (Sz len) $ arrRLightIx2 DL Par sz
  defaultMain
    [ mkAppendBenchGroup "LeftToRight" (Dim 1) sz
    , mkAppendBenchGroup "TopToBottom" (Dim 2) sz
    , bgroup
        "Monoid"
        [ bench "mappend" $ whnf (\a -> A.computeAs P (toLoadArray a <> toLoadArray a)) arr
        , bench "appendDL" $
          whnfIO (A.computeAs P <$> appendOuterM (toLoadArray arr) (toLoadArray arr))
        , bench "mconcat" $ whnf (\a -> A.computeAs P (mconcat [toLoadArray a, toLoadArray a])) arr
        ]
    , bgroup
        "cons"
        [ bench ("Array DL Ix1 Int (" ++ show kSmall ++ ")") $
          nf (A.computeAs P . consArray kSmall) empty
        -- , bench ("Array DS Ix1 Int (" ++ show kSmall ++ ")") $
          --   nf (A.computeAs P . sconsArray kSmall) empty
        , bench ("VP.Vector Int (" ++ show kSmall ++ ")") $ nf (consVector kSmall) VP.empty
        , bench ("[Int] (" ++ show kSmall ++ ")") $ nf (consList kSmall) []
        ]
    , bgroup
        "uncons"
        [ bench ("Array P Ix1 Int (" ++ show kSmall ++ ")") $ nf (unconsArray kSmall) arr
        -- , bench ("Array DS Ix1 Int (" ++ show kSmall ++ ")") $
        --   nf (sunconsArray kSmall . toStreamArray) arr
        , env (pure (A.toVector arr :: VP.Vector Double)) $ \v ->
            bench ("VP.Vector Int (" ++ show kSmall ++ ")") $ nf (unconsVector kSmall) v
        , env (pure (toList arr :: [Double])) $ \xs ->
            bench ("[Int] (" ++ show kSmall ++ ")") $ nf (unconsList kSmall) xs
        ]
    , bgroup
        "snoc"
        [ bench ("Array DL Ix1 Int (" ++ show kSmall ++ ")") $
          nf (A.computeAs P . snocArray kSmall) empty
        , bench ("VP.Vector Int (" ++ show kSmall ++ ")") $ nf (snocVector kSmall) VP.empty
        , bench ("DList Int (" ++ show kSmall ++ ")") $ nf (snocList kSmall) DL.empty
        ]
    , bgroup
        "unfoldr"
        [ bench "Array (DS)" $ whnf (A.computeAs P . A.sunfoldr firstK) 0
        , bench "Vector" $ whnf (VP.unfoldr firstK) 0
        ]
    , bgroup
        "unfoldrN"
        [ bench "Array (DL)" $ whnf (A.computeAs P . unfoldrS_ (Sz k) (\i -> (i :: Int, i + 1))) 0
        , bench "Array (DS)" $
          whnf (A.computeAs P . A.sunfoldrN (Sz k) (\i -> Just (i :: Int, i + 1))) 0
        , bench "Vector" $ whnf (VP.unfoldrN k (\i -> Just (i :: Int, i + 1))) 0
        ]
    ]
  where
    !k = 1000000 :: Int
    !kSmall = 10000 :: Int
    firstK i =
      if i < k
        then Just (i, i + 1)
        else Nothing
    consList :: Int -> [Int] -> [Int]
    consList 0 !acc = acc
    consList !n !acc = consList (n - 1) (n : acc)
    consArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
    consArray 0 !acc = acc
    consArray !n !acc = consArray (n - 1) (n `A.cons` acc)
    -- sconsArray :: Int -> Array DS Ix1 Int -> Array DS Ix1 Int
    -- sconsArray 0 !acc = acc
    -- sconsArray !n !acc = sconsArray (n - 1) (n `A.scons` acc)
    consVector :: Int -> VP.Vector Int -> VP.Vector Int
    consVector 0 !acc = acc
    consVector !n !acc = consVector (n - 1) (n `VP.cons` acc)
    snocList :: Int -> DL.DList Int -> [Int]
    snocList 0 !acc = DL.toList acc
    snocList !n !acc = snocList (n - 1) (acc `DL.snoc` n)
    snocArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
    snocArray 0 !acc = acc
    snocArray !n !acc = snocArray (n - 1) (acc `A.snoc` n)
    snocVector :: Int -> VP.Vector Int -> VP.Vector Int
    snocVector 0 !acc = acc
    snocVector !n !acc = snocVector (n - 1) (acc `VP.snoc` n)
    unconsArray n arr
      | 1 < n =
        case unconsM arr of
          Nothing -> error "Unexpected end of delayed array"
          Just (!_e, arr') -> unconsArray (n - 1) arr'
      | otherwise = fst $ fromJust $ unconsM arr
    -- sunconsArray n arr
    --   | 1 < n =
    --     case sunconsM arr of
    --       Nothing -> error "Unexpected end of stream array"
    --       Just (!_e, arr') -> sunconsArray (n - 1) arr'
    --   | otherwise = fst $ fromJust $ sunconsM arr
    unconsVector :: Prim a => Int -> VP.Vector a -> a
    unconsVector n xs
      | 1 < n =
        case unconsVP xs of
          Nothing -> error "Unexpected end of vector"
          Just (!_e, xs') -> unconsVector (n - 1) xs'
      | otherwise = fst $ fromJust $ unconsVP xs
    unconsList :: Int -> [a] -> a
    unconsList n xs
      | 1 < n =
        case List.uncons xs of
          Nothing -> error "Unexpected end of list"
          Just (!_e, xs') -> unconsList (n - 1) xs'
      | otherwise = fst $ fromJust $ List.uncons xs

mkAppendBenchGroup :: String -> Dim -> Sz2 -> Benchmark
mkAppendBenchGroup gname dim sz =
  bgroup
    ("Append " ++ gname)
    [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
        bgroup
          "Seq"
          [ bench "append" $ whnf (A.computeAs P . append' dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . A.concat' dim . (: [arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    , env (return (arrRLightIx2 P Par sz)) $ \arr ->
        bgroup
          "Par"
          [ bench "append" $ whnf (A.computeAs P . append' dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . A.concat' dim . (: [arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    ]


unconsVP :: Prim a => VP.Vector a -> Maybe (a, VP.Vector a)
unconsVP xs = flip (,) (VP.unsafeTail xs) <$> xs VP.!? 0
