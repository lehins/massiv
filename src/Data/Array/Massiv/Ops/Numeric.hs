{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Delayed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Numeric
  -- * Num
  ( (.+)
  , (.-)
  , (.*)
  , (.^)
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
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Ops.Fold     as M
import           Data.Array.Massiv.Ops.Map      as M
import           Prelude                        as P


infixr 8  .^, .^^
infixl 7  ./, `quotA`, `remA`, `divA`, `modA`

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


(|*|)
  :: (Manifest r1 ix e, Manifest r2 ix e, Num e, Slice r1 ix e, Slice r2 ix e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
(|*|) arr1 arr2
  | getIndex n1 1 /= Just m2 =
      error "(|*|)" $
         "Inner array dimensions must agree, but received: " ++ show arr1 ++ " and " ++ show arr2
  -- | not (isSafeSize sz1 || isSafeSize sz2) =
  --     error "(|*|)" $
  --        "Inner array dimensions must agree, but received: " ++ show arr1 ++ " and " ++ show arr2
  | otherwise = DArray (consDim m1 n2) $ \ !ix -> sumS ((arr1 !> headDim ix) .* (arr2 <! lastDim ix))
  where (m1, n1) = unconsDim $ size arr1
        (m2, n2) = unconsDim $ size arr2
{-# INLINE (|*|) #-}

negateA
  :: (Source r ix e, Num e)
  => Array r ix e -> Array D ix e
negateA = liftArray abs
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
fromIntegerA = singleton . fromInteger
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
fromRationalA = singleton . fromRational
{-# INLINE fromRationalA #-}

piA
  :: (Index ix, Floating e)
  => Array D ix e
piA = singleton pi
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
quotRemA arr1 = M.unzip . liftArray2 (quotRem) arr1
{-# INLINE quotRemA #-}


divModA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> (Array D ix e, Array D ix e)
divModA arr1 = M.unzip . liftArray2 (divMod) arr1
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

