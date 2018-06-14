{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Numeric
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Numeric
  ( -- * Num
    (.+)
  , (.-)
  , (.*)
  , (.^)
  , (|*|)
  , negateA
  , absA
  , signumA
  , fromIntegerA
  -- * Integral
  , quotA
  , remA
  , divA
  , modA
  , quotRemA
  , divModA
  -- * Fractional
  , (./)
  , (.^^)
  , recipA
  , fromRationalA
  -- * Floating
  , piA
  , expA
  , logA
  , sqrtA
  , (.**)
  , logBaseA
  , sinA
  , cosA
  , tanA
  , asinA
  , acosA
  , atanA
  , sinhA
  , coshA
  , tanhA
  , asinhA
  , acoshA
  , atanhA
  -- * RealFrac
  , truncateA
  , roundA
  , ceilingA
  , floorA
  -- * RealFloat
  , atan2A
  , inverse
  , det
  , subAt
  ) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Massiv.Array.Manifest.List as A
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Ops.Fold         as A
import           Data.Massiv.Array.Manifest.Internal (compute)
import           Data.Massiv.Array.Ops.Map          as A
import           Data.Massiv.Array.Ops.Slice        as A
import           Data.Massiv.Array.Ops.Transform    as A
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Data.Monoid                        ((<>))
import           Prelude                            as P

type Matrix r e = Array r Ix2 e

inverse
  :: forall e r.
     ( Mutable r Ix2 e
     , Fractional e
     , Num e
     ) => Matrix r e -> Maybe (Matrix r e)
inverse arr
  | (rs :. cs) <- size arr
  , rs /= cs = Nothing
  | 1 :. 1 <- size arr = do
    let dt = 1 / (arr ! (0 :. 0))
    pure $ fromLists' Seq [[dt]]
  | size arr == 2 :. 2 =
    let
        a = arr ! (0 :. 0)
        b = arr ! (0 :. 1)
        c = arr ! (1 :. 0)
        d = arr ! (1 :. 1)
        dt = 1 / (a*d - b*c)
        ls = fmap (fmap (/dt)) [[d, -b], [-c, a]]
    in pure $ fromLists' Seq ls
  | otherwise = Just $ compute $ imap ( \(j :. i) e ->
      let scale = if odd $ j + i then 1 else -1
      in e

      ) arr

  {-
      generateM (size arr) (\(i :. j) -> do
        let v = arr ! (j :. i)
        undefined
  -}
        -- v' <- det (subAt


-- matrixOfCoFactors
  -- :: Matrix r e -> Maybe (Matrix r e)
-- matrixOfCoFactors =



det :: (Manifest r Ix2 e, Fractional e, Mutable r Ix2 e) => Matrix r e -> Maybe e
det arr
  | rs /= cs = Nothing
  | rs :. cs == 1 :. 1 = Just $ arr ! (0 :. 0)
  | rs :. cs == 2 :. 2 =
    let
        a = arr ! (0 :. 0)
        b = arr ! (0 :. 1)
        c = arr ! (1 :. 0)
        d = arr ! (1 :. 1)
    in pure $ (a*d - b*c)
  | otherwise =
      fix
        (\f j acc ->
          case arr !? (0 :. j) of
            Nothing -> acc
            Just e -> do
              let op = liftA2 $ if even j then (+) else (-)
              let term = (e *) <$>  minorAt (0 :. j) arr -- det (subAt (0 :. j) arr)
              f (j+1) $ acc `op` term
        )
        0  -- start index
        (Just 0) -- determinant
  where
    rs :. cs = size arr

minorAt :: (Manifest r Ix2 e, Fractional e, Mutable r Ix2 e) => Ix2 -> Matrix r e -> Maybe e
minorAt ix arr
    | rs /= cs = Nothing
    | otherwise =det $ subAt ix arr
  where
    rs :. cs = size arr

-- | Given index (i, j) computes the submatrix formed by removing row i and
-- column j.
subAt :: Mutable r Ix2 e => Ix2 -> Matrix r e -> Matrix r e
subAt (dr :. dc) arr = compute $ backpermute (size arr - 1 ) f arr
  where
    f (row :. col) = (skipOne dr row :. skipOne dc col)
    skipOne xDropped x
      | x < xDropped = x
      | otherwise = x + 1

-- submatrix :: Ix2 -> Matrix r e -> Matrix r e
-- submatrix = undefined

infixr 8  .^, .^^
infixl 7  .*, ./, `quotA`, `remA`, `divA`, `modA`
infixl 6  .+, .-

(.+)
  :: (Source r1 ix e, Source r2 ix e, Num e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
(.+) = liftArray2 (+)
{-# INLINE (.+) #-}

(.-)
  :: (Source r1 ix e, Source r2 ix e, Num e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
(.-) = liftArray2 (-)
{-# INLINE (.-) #-}

(.*)
  :: (Source r1 ix e, Source r2 ix e, Num e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
(.*) = liftArray2 (*)
{-# INLINE (.*) #-}

(.^)
  :: (Source r ix e, Num e, Integral b)
  => Array r ix e -> b -> Array D ix e
(.^) arr n = liftArray (^ n) arr
{-# INLINE (.^) #-}

-- | Perform matrix multiplication. Inner dimensions must agree, otherwise error.
(|*|) ::
     ( Mutable r1 Ix2 e
     , Mutable r2 Ix2 e
     , OuterSlice r1 Ix2 e
     , OuterSlice r2 Ix2 e
     , Source (EltRepr r1 Ix2) Ix1 e
     , Source (EltRepr r2 Ix2) Ix1 e
     , Num e
     )
  => Array r1 Ix2 e
  -> Array r2 Ix2 e
  -> Array D Ix2 e
(|*|) = multArrs
{-# INLINE (|*|) #-}


multArrs :: forall r1 r2 e.
            ( Mutable r1 Ix2 e
            , Mutable r2 Ix2 e
            , OuterSlice r1 Ix2 e
            , OuterSlice r2 Ix2 e
            , Source (EltRepr r1 Ix2) Ix1 e
            , Source (EltRepr r2 Ix2) Ix1 e
            , Num e
            )
         => Array r1 Ix2 e -> Array r2 Ix2 e -> Array D Ix2 e
multArrs arr1 arr2
  | n1 /= m2 =
    error $
    "(|*|): Inner array dimensions must agree, but received: " ++
    show (size arr1) ++ " and " ++ show (size arr2)
  | otherwise =
    DArray (getComp arr1 <> getComp arr2) (m1 :. n2) $ \(i :. j) ->
      A.sum ((arr1' !> i) .* (arr2' !> j))
  where
    (m1 :. n1) = size arr1
    (m2 :. n2) = size arr2
    arr1' = setComp Seq arr1
    arr2' :: Array r2 Ix2 e
    arr2' = setComp Seq $ compute $ transpose arr2
{-# INLINE multArrs #-}


negateA
  :: (Source r ix e, Num e)
  => Array r ix e -> Array D ix e
negateA = liftArray negate
{-# INLINE negateA #-}

absA
  :: (Source r ix e, Num e)
  => Array r ix e -> Array D ix e
absA = liftArray abs
{-# INLINE absA #-}

signumA
  :: (Source r ix e, Num e)
  => Array r ix e -> Array D ix e
signumA = liftArray signum
{-# INLINE signumA #-}

fromIntegerA
  :: (Index ix, Num e)
  => Integer -> Array D ix e
fromIntegerA = singleton Seq . fromInteger
{-# INLINE fromIntegerA #-}

(./)
  :: (Source r1 ix e, Source r2 ix e, Fractional e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
(./) = liftArray2 (/)
{-# INLINE (./) #-}

(.^^)
  :: (Source r ix e, Fractional e, Integral b)
  => Array r ix e -> b -> Array D ix e
(.^^) arr n = liftArray (^^ n) arr
{-# INLINE (.^^) #-}

recipA
  :: (Source r ix e, Fractional e)
  => Array r ix e -> Array D ix e
recipA = liftArray recip
{-# INLINE recipA #-}


fromRationalA
  :: (Index ix, Fractional e)
  => Rational -> Array D ix e
fromRationalA = singleton Seq . fromRational
{-# INLINE fromRationalA #-}

piA
  :: (Index ix, Floating e)
  => Array D ix e
piA = singleton Seq pi
{-# INLINE piA #-}

expA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
expA = liftArray exp
{-# INLINE expA #-}

sqrtA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
sqrtA = liftArray exp
{-# INLINE sqrtA #-}

logA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
logA = liftArray log
{-# INLINE logA #-}

logBaseA
  :: (Source r1 ix e, Source r2 ix e, Floating e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
logBaseA = liftArray2 logBase
{-# INLINE logBaseA #-}

(.**)
  :: (Source r1 ix e, Source r2 ix e, Floating e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
(.**) = liftArray2 (**)
{-# INLINE (.**) #-}



sinA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
sinA = liftArray sin
{-# INLINE sinA #-}

cosA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
cosA = liftArray cos
{-# INLINE cosA #-}

tanA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
tanA = liftArray cos
{-# INLINE tanA #-}

asinA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
asinA = liftArray asin
{-# INLINE asinA #-}

atanA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
atanA = liftArray atan
{-# INLINE atanA #-}

acosA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
acosA = liftArray acos
{-# INLINE acosA #-}

sinhA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
sinhA = liftArray sinh
{-# INLINE sinhA #-}

tanhA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
tanhA = liftArray cos
{-# INLINE tanhA #-}

coshA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
coshA = liftArray cosh
{-# INLINE coshA #-}

asinhA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
asinhA = liftArray asinh
{-# INLINE asinhA #-}

acoshA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
acoshA = liftArray acosh
{-# INLINE acoshA #-}

atanhA
  :: (Source r ix e, Floating e)
  => Array r ix e -> Array D ix e
atanhA = liftArray atanh
{-# INLINE atanhA #-}


quotA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
quotA = liftArray2 (quot)
{-# INLINE quotA #-}


remA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
remA = liftArray2 (rem)
{-# INLINE remA #-}

divA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
divA = liftArray2 (div)
{-# INLINE divA #-}

modA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
modA = liftArray2 (mod)
{-# INLINE modA #-}



quotRemA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> (Array D ix e, Array D ix e)
quotRemA arr1 = A.unzip . liftArray2 (quotRem) arr1
{-# INLINE quotRemA #-}


divModA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> (Array D ix e, Array D ix e)
divModA arr1 = A.unzip . liftArray2 (divMod) arr1
{-# INLINE divModA #-}



truncateA
  :: (Source r ix a, RealFrac a, Integral b)
  => Array r ix a -> Array D ix b
truncateA = liftArray truncate
{-# INLINE truncateA #-}


roundA
  :: (Source r ix a, RealFrac a, Integral b)
  => Array r ix a -> Array D ix b
roundA = liftArray round
{-# INLINE roundA #-}


ceilingA
  :: (Source r ix a, RealFrac a, Integral b)
  => Array r ix a -> Array D ix b
ceilingA = liftArray ceiling
{-# INLINE ceilingA #-}


floorA
  :: (Source r ix a, RealFrac a, Integral b)
  => Array r ix a -> Array D ix b
floorA = liftArray floor
{-# INLINE floorA #-}

atan2A
  :: (Source r ix e, RealFloat e)
  => Array r ix e -> Array r ix e -> Array D ix e
atan2A = liftArray2 atan2
{-# INLINE atan2A #-}

