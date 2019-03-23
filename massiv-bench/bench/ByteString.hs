{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ByteString as R
import Data.Bits as Bits
import qualified Data.ByteString as S
import Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy as SL
import qualified Data.ByteString.Unsafe as SU
import Data.Functor.Identity
import Data.Int
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import System.Random (mkStdGen, randoms)


convertWords :: Int16 -> Int16 -> Int16
convertWords !word1 !word2 = (word1 `shiftL` 8) .|. word2

-- | Answer to stackoverflow question:
-- https://stackoverflow.com/questions/48218110/repa-creation-from-bytestring/51629720#51629720
_bytesToRepaOriginal :: S.ByteString -> R.Array R.U R.DIM1 Int16
_bytesToRepaOriginal bs =
  R.foldS
    convertWords
    0
    (R.map fromIntegral $
     R.fromByteString (R.Z R.:. (S.length bs `div` 2) R.:. 2) bs)


bytesToRepaP :: Monad m => S.ByteString -> m (R.Array R.U R.DIM1 Int16)
bytesToRepaP bs =
  R.computeUnboxedP $
  R.fromFunction
    (R.Z R.:. (S.length bs `div` 2))
    (\(R.Z R.:. i) ->
       let i' = i * 2
           f = SU.unsafeIndex bs
        in (fromIntegral (f i') `shiftL` 8) .|. fromIntegral (f (i' + 1)))

bytesToRepa :: S.ByteString -> R.Array R.U R.DIM1 Int16
bytesToRepa bs =
  R.computeUnboxedS $
  R.fromFunction
    (R.Z R.:. (S.length bs `div` 2))
    (\(R.Z R.:. i) ->
       let i' = i * 2
           f = SU.unsafeIndex bs
        in (fromIntegral (f i') `shiftL` 8) .|. fromIntegral (f (i' + 1)))

bytesToMassiv :: Comp -> S.ByteString -> A.Array A.U A.Ix1 Int16
bytesToMassiv comp bs =
  A.makeArrayR U comp (S.length bs `div` 2)
    (\i ->
       let i' = i * 2
           f = SU.unsafeIndex bs
        in (fromIntegral (f i') `shiftL` 8) .|. fromIntegral (f (i' + 1)))

bytesToMassiv' :: Comp -> S.ByteString -> A.Array A.U A.Ix1 Int16
bytesToMassiv' comp bs =
  A.makeArrayR U comp (S.length bs `div` 2)
    (\i ->
       let i' = i * 2
           arr = fromByteString comp bs
           f = A.unsafeIndex arr
        in (fromIntegral (f i') `shiftL` 8) .|. fromIntegral (f (i' + 1)))


main :: IO ()
main = do
  let initialSeed, numOfBytes :: Int
      initialSeed = 2018
      numOfBytes = 1024 * 1024 -- 1MiB
      bs = S.pack $ take numOfBytes $ randoms $ mkStdGen initialSeed
  defaultMain
    [ bgroup
        "toByteString"
        [ env
            (return bs)
            (bench "Massiv (toBuilder)" .
             whnf (SL.toStrict . S.toLazyByteString . toBuilder S.word8 . fromByteString Seq))
        , env (return bs) (bench "Massiv (toByteString)" . whnf (toByteString . fromByteString Seq))
        , env
            (return bs)
            (bench "Native through Builder" . whnf (SL.toStrict . S.toLazyByteString . S.byteString))
        ]
    , bgroup
        "SO - fromByteString (Parallel)"
        [ env (return bs) (bench "Massiv" . whnf (bytesToMassiv Par))
        , env (return bs) (bench "Massiv (fromByteString)" . whnf (bytesToMassiv' Par))
        , env (return bs) (bench "Repa" . whnf (runIdentity . bytesToRepaP))
          --, env (return bs) (bench "Repa stackoverflow" . whnf _bytesToRepaOriginal)
        ]
    ]
