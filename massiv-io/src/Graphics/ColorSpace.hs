{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Graphics.ColorSpace
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace (
  -- * Pixels
  -- ** Family of Pixels
  -- | Pixel is a type family for all available color spaces. Below is the
  -- listed of all class instances, that pixels are installed in, as well as all
  -- pixel constructors.
  --
  -- >>> :t (PixelY 0) -- Black pixel in Luma
  -- (PixelY 0) :: Num e => Pixel Y e
  -- >>> PixelRGB 255 0 0 :: Pixel RGB Word8 -- Red pixel in RGB
  -- <RGB:(255|0|0)>
  -- >>> PixelRGB 1 0 0 :: Pixel RGB Double -- Same red pixel in RGB with Double precision.
  -- <RGB:(1.0|0.0|0.0)>
  -- >>> (PixelRGB 255 0 0 :: Pixel RGB Word8) == (toWord8 <$> (PixelRGB 1 0 0 :: Pixel RGB Double))
  -- True
  --
  Pixel(..)
  , ColorSpace(..)
  , AlphaSpace(..)
  -- ** Luma (gray scale)
  -- | Conversion to Luma from other color spaces.
  , toPixelY
  , toPixelYA
  -- ** RGB
  -- | Conversion to RGB from other color spaces.
  , toPixelRGB
  , toPixelRGBA
  -- ** HSI
  -- | Conversion to HSI from other color spaces.
  , toPixelHSI
  , toPixelHSIA
  -- ** CMYK
  -- | Conversion to CMYK from other color spaces.
  , toPixelCMYK
  , toPixelCMYKA
  -- ** YCbCr
  -- | Conversion to YCbCr from other color spaces.
  , toPixelYCbCr
  , toPixelYCbCrA
  -- ** Binary
  -- | This is a Binary colorspace, pixel's of which can be created using
  -- these __/constructors/__:
  --
  --   [@'on'@] Represents value @1@ or 'True'. It's a foreground pixel and is
  --   displayed in black.
  --
  --   [@'off'@] Represents value @0@ or 'False'. It's a background pixel and is
  --   displayed in white.
  --
  -- Note, that values are inverted before writing to or reading from file, since
  -- grayscale images represent black as a @0@ value and white as @1@ on a
  -- @[0,1]@ scale.
  --
  -- Binary pixels also behave as binary numbers with a size of 1-bit, for instance:
  --
  -- >>> on + on -- equivalent to: 1 .|. 1
  -- <Binary:(1)>
  -- >>> (on + on) * off -- equivalent to: (1 .|. 1) .&. 0
  -- <Binary:(0)>
  -- >>> (on + on) - on
  -- <Binary:(0)>
  --
  , toPixelBinary
  , fromPixelBinary
  , module Graphics.ColorSpace.Binary
  -- ** Complex
  , module Graphics.ColorSpace.Complex
  -- ** X
  , toPixelsX
  , fromPixelsX
  -- * ColorSpace
  -- ** Operations on Pixels
  , eqTolPx
  -- ** Luma
  ,Y(..), YA(..),
  ToY, ToYA,
  -- ** RGB
  RGB(..), RGBA(..),
  ToRGB, ToRGBA,
  -- ** HSI
  HSI(..), HSIA(..),
  ToHSI, ToHSIA,
  -- ** CMYK
  CMYK(..), CMYKA(..),
  ToCMYK, ToCMYKA,
  -- ** YCbCr
  YCbCr(..), YCbCrA(..),
  ToYCbCr, ToYCbCrA,
  -- ** X
  X(..),
  -- * Precision
  -- ** Pixel
  toWord8,
  toWord16,
  toWord32,
  toWord64,
  toFloat,
  toDouble,
  fromDouble,
  -- ** Componenet
  Word8, Word16, Word32, Word64
  ) where

import           Data.Word
import           Graphics.ColorSpace.Binary
import           Graphics.ColorSpace.CMYK
import           Graphics.ColorSpace.Complex
import           Graphics.ColorSpace.HSI
import           Graphics.ColorSpace.Internal
import           Graphics.ColorSpace.RGB
import           Graphics.ColorSpace.X
import           Graphics.ColorSpace.Y
import           Graphics.ColorSpace.YCbCr


-- -- Binary:

-- | Convert to a `Binary` pixel.
toPixelBinary :: ColorSpace cs e => Pixel cs e -> Pixel X Bit
toPixelBinary px = if px == 0 then on else off


-- | Convert a Binary pixel to Luma pixel
fromPixelBinary :: Pixel X Bit -> Pixel Y Word8
fromPixelBinary b = PixelY $ if isOn b then minBound else maxBound
{-# INLINE fromPixelBinary #-}


-- | Check weather two Pixels are equal within a tolerance. Useful for comparing
-- pixels with `Float` or `Double` precision.
eqTolPx :: (ColorSpace cs e, Ord e) =>
           e -> Pixel cs e -> Pixel cs e -> Bool
eqTolPx !tol = foldlPx2 comp True
  where comp !acc !e1 !e2 = acc && max e1 e2 - min e1 e2 <= tol
        {-# INLINE comp #-}
{-# INLINE eqTolPx #-}

-- ToY

-- | Conversion to Luma color space.
class ColorSpace cs e => ToY cs e where

  -- | Convert a pixel to Luma pixel.
  toPixelY :: Pixel cs e -> Pixel Y Double


instance Elevator e => ToY X e where
  toPixelY (PixelX y) = PixelY $ eToDouble y
  {-# INLINE toPixelY #-}

instance Elevator e => ToY Y e where
  toPixelY (PixelY y) = PixelY $ eToDouble y
  {-# INLINE toPixelY #-}

instance Elevator e => ToY YA e where
  toPixelY (PixelYA y _) = PixelY $ eToDouble y
  {-# INLINE toPixelY #-}

-- | Computes Luma: @ Y' = 0.299 * R' + 0.587 * G' + 0.114 * B' @
instance Elevator e => ToY RGB e where
  toPixelY (toDouble -> PixelRGB r g b) = PixelY (0.299*r + 0.587*g + 0.114*b)
  {-# INLINE toPixelY #-}

instance Elevator e => ToY RGBA e where
  toPixelY = toPixelY . dropAlpha
  {-# INLINE toPixelY #-}

instance Elevator e => ToY HSI e where
  toPixelY = toPixelY . toPixelRGB . toDouble
  {-# INLINE toPixelY #-}

instance Elevator e => ToY HSIA e where
  toPixelY = toPixelY . dropAlpha
  {-# INLINE toPixelY #-}

instance Elevator e => ToY CMYK e where
  toPixelY = toPixelY . toPixelRGB . toDouble
  {-# INLINE toPixelY #-}

instance Elevator e => ToY CMYKA e where
  toPixelY = toPixelY . toPixelRGB . toDouble . dropAlpha
  {-# INLINE toPixelY #-}

instance Elevator e => ToY YCbCr e where
  toPixelY (PixelYCbCr y _ _) = PixelY $ eToDouble y
  {-# INLINE toPixelY #-}

instance Elevator e => ToY YCbCrA e where
  toPixelY (PixelYCbCrA y _ _ _) = PixelY $ eToDouble y
  {-# INLINE toPixelY #-}


-- ToYA

-- | Conversion to Luma from another color space.
class ToY cs e => ToYA cs e where

  -- | Convert a pixel to Luma pixel with Alpha.
  toPixelYA :: Pixel cs e -> Pixel YA Double
  toPixelYA = addAlpha 1 . toPixelY
  {-# INLINE toPixelYA #-}


instance ToYA X Bit where
  toPixelYA (PixelX y) = PixelYA (eToDouble y) 1
  {-# INLINE toPixelYA #-}


instance ToY Y e => ToYA Y e

instance Elevator e => ToYA YA e where
  toPixelYA = toDouble
  {-# INLINE toPixelYA #-}

instance ToY RGB e => ToYA RGB e

instance Elevator e => ToYA RGBA e where
  toPixelYA !px = addAlpha (eToDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

instance ToY HSI e => ToYA HSI e

instance Elevator e => ToYA HSIA e where
  toPixelYA !px = addAlpha (eToDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

instance ToY CMYK e => ToYA CMYK e

instance Elevator e => ToYA CMYKA e where
  toPixelYA !px = addAlpha (eToDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

instance ToY YCbCr e => ToYA YCbCr e

instance Elevator e => ToYA YCbCrA e where
  toPixelYA !px = addAlpha (eToDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

-- ToRGB



-- | Conversion to `RGB` color space.
class ColorSpace cs e => ToRGB cs e where

  -- | Convert to an `RGB` pixel.
  toPixelRGB :: Pixel cs e -> Pixel RGB Double


instance ToRGB X Bit where
  toPixelRGB (PixelX b) = promote $ eToDouble b
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB Y e where
  toPixelRGB (PixelY g) = promote $ eToDouble g
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB YA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB RGB e where
  toPixelRGB = toDouble
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB RGBA e where
  toPixelRGB = toDouble . dropAlpha
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB HSI e where
  toPixelRGB (toDouble -> PixelHSI h' s i) = getRGB (h'*2*pi) where
    !is = i*s
    !second = i - is
    errorHue = error $ "HSI pixel is not properly scaled, Hue: " ++ show h'
    getFirst !a !b = i + is*cos a/cos b
    {-# INLINE getFirst #-}
    getThird !v1 !v2 = i + 2*is + v1 - v2
    {-# INLINE getThird #-}
    getRGB h
      | h < 0      = errorHue
      | h < 2*pi/3 = let !r = getFirst h (pi/3 - h)
                         !b = second
                         !g = getThird b r
                     in PixelRGB r g b
      | h < 4*pi/3 = let !g = getFirst (h - 2*pi/3) (h + pi)
                         !r = second
                         !b = getThird r g
                     in PixelRGB r g b
      | h < 2*pi   = let !b = getFirst (h - 4*pi/3) (2*pi - pi/3 - h)
                         !g = second
                         !r = getThird g b
                     in PixelRGB r g b
      | otherwise  = errorHue
    {-# INLINE getRGB #-}
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB HSIA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}


instance Elevator e => ToRGB YCbCr e where
  toPixelRGB (toDouble -> PixelYCbCr y cb cr) = PixelRGB r g b where
    !r = clamp01 (y                      +   1.402*(cr - 0.5))
    !g = clamp01 (y - 0.34414*(cb - 0.5) - 0.71414*(cr - 0.5))
    !b = clamp01 (y +   1.772*(cb - 0.5))
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB YCbCrA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB CMYK e where
  toPixelRGB (toDouble -> PixelCMYK c m y k) = PixelRGB r g b where
    !r = (1-c)*(1-k)
    !g = (1-m)*(1-k)
    !b = (1-y)*(1-k)
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB CMYKA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}


-- ToRGBA

-- | Conversion to `RGBA` from another color space with Alpha channel.
class ToRGB cs e => ToRGBA cs e where

  -- | Convert to an `RGBA` pixel.
  toPixelRGBA :: Pixel cs e -> Pixel RGBA Double
  toPixelRGBA = addAlpha 1 . toPixelRGB
  {-# INLINE toPixelRGBA #-}


instance ToRGBA X Bit

instance ToRGB Y e => ToRGBA Y e

instance Elevator e => ToRGBA YA e where
  toPixelRGBA !px = addAlpha (eToDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}

instance ToRGB RGB e => ToRGBA RGB e

instance Elevator e => ToRGBA RGBA e where
  toPixelRGBA = toDouble
  {-# INLINE toPixelRGBA #-}

instance ToRGB HSI e => ToRGBA HSI e

instance Elevator e => ToRGBA HSIA e where
  toPixelRGBA !px = addAlpha (eToDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}

instance ToRGB CMYK e => ToRGBA CMYK e

instance Elevator e => ToRGBA CMYKA e where
  toPixelRGBA !px = addAlpha (eToDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}

instance ToRGB YCbCr e => ToRGBA YCbCr e

instance Elevator e => ToRGBA YCbCrA e where
  toPixelRGBA !px = addAlpha (eToDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}


-- ToHSI

-- | Conversion to `HSI` color space.
class ColorSpace cs e => ToHSI cs e where

  -- | Convert to an `HSI` pixel.
  toPixelHSI :: Pixel cs e -> Pixel HSI Double


instance Elevator e => ToHSI Y e where
  toPixelHSI (PixelY y) = PixelHSI 0 0 $ eToDouble y
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI YA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI RGB e where
  toPixelHSI (toDouble -> PixelRGB r g b) = PixelHSI h s i where
    !h' = atan2 y x
    !h = (if h' < 0 then h' + 2*pi else h') / (2*pi)
    !s = if i == 0 then 0 else 1 - minimum [r, g, b] / i
    !i = (r + g + b) / 3
    !x = (2*r - g - b) / 2.449489742783178
    !y = (g - b) / 1.4142135623730951
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI RGBA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI HSI e where
  toPixelHSI = toDouble
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI HSIA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI YCbCr e where
  toPixelHSI = toPixelHSI . toPixelRGB
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI YCbCrA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI CMYK e where
  toPixelHSI = toPixelHSI . toPixelRGB
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI CMYKA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}


-- ToHSIA

-- | Conversion to `HSIA` from another color space with Alpha channel.
class ToHSI cs e => ToHSIA cs e where

  -- | Convert to an `HSIA` pixel.
  toPixelHSIA :: Pixel cs e -> Pixel HSIA Double
  toPixelHSIA = addAlpha 1 . toPixelHSI
  {-# INLINE toPixelHSIA #-}


instance ToHSI Y e => ToHSIA Y e

instance Elevator e => ToHSIA YA e where
  toPixelHSIA !px = addAlpha (eToDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}

instance ToHSI RGB e => ToHSIA RGB e

instance Elevator e => ToHSIA RGBA e where
  toPixelHSIA !px = addAlpha (eToDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}

instance ToHSI HSI e => ToHSIA HSI e

instance Elevator e => ToHSIA HSIA e where
  toPixelHSIA = toDouble
  {-# INLINE toPixelHSIA #-}

instance ToHSI CMYK e => ToHSIA CMYK e

instance Elevator e => ToHSIA CMYKA e where
  toPixelHSIA !px = addAlpha (eToDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}

instance ToHSI YCbCr e => ToHSIA YCbCr e

instance Elevator e => ToHSIA YCbCrA e where
  toPixelHSIA !px = addAlpha (eToDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}



-- ToCMYK

-- | Conversion to `CMYK` color space.
class ColorSpace cs e => ToCMYK cs e where

  -- | Convert to a `CMYK` pixel.
  toPixelCMYK :: Pixel cs e -> Pixel CMYK Double


instance Elevator e => ToCMYK Y e where
  toPixelCMYK = toPixelCMYK . toPixelRGB
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK YA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK RGB e where
  toPixelCMYK (toDouble -> PixelRGB r g b) = PixelCMYK c m y k where
    !c = (1 - r - k)/(1 - k)
    !m = (1 - g - k)/(1 - k)
    !y = (1 - b - k)/(1 - k)
    !k = 1 - max r (max g b)
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK RGBA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK HSI e where
  toPixelCMYK = toPixelCMYK . toPixelRGB
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK HSIA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK CMYK e where
  toPixelCMYK = toDouble
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK CMYKA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK YCbCr e where
  toPixelCMYK = toPixelCMYK . toPixelRGB
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK YCbCrA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}


-- ToCMYKA

-- | Conversion to `CMYKA`.
class ToCMYK cs e => ToCMYKA cs e where

  -- | Convert to a `CMYKA` pixel.
  toPixelCMYKA :: Pixel cs e -> Pixel CMYKA Double
  toPixelCMYKA = addAlpha 1 . toPixelCMYK
  {-# INLINE toPixelCMYKA #-}


instance ToCMYK Y e => ToCMYKA Y e

instance Elevator e => ToCMYKA YA e where
  toPixelCMYKA !px = addAlpha (eToDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK RGB e => ToCMYKA RGB e

instance Elevator e => ToCMYKA RGBA e where
  toPixelCMYKA !px = addAlpha (eToDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK HSI e => ToCMYKA HSI e

instance Elevator e => ToCMYKA HSIA e where
  toPixelCMYKA !px = addAlpha (eToDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK CMYK e => ToCMYKA CMYK e

instance Elevator e => ToCMYKA CMYKA e where
  toPixelCMYKA = toDouble
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK YCbCr e => ToCMYKA YCbCr e

instance Elevator e => ToCMYKA YCbCrA e where
  toPixelCMYKA !px = addAlpha (eToDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}




-- ToYCbCr

-- | Conversion to `YCbCr` color space.
class ColorSpace cs e => ToYCbCr cs e where

  -- | Convert to an `YCbCr` pixel.
  toPixelYCbCr :: Pixel cs e -> Pixel YCbCr Double



instance Elevator e => ToYCbCr Y e where
  toPixelYCbCr = toPixelYCbCr . toPixelRGB
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr YA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr RGB e where
  toPixelYCbCr (toDouble -> PixelRGB r g b) = PixelYCbCr y cb cr where
    !y  = clamp01 (         0.299*r +    0.587*g +    0.114*b)
    !cb = clamp01 (0.5 - 0.168736*r - 0.331264*g +      0.5*b)
    !cr = clamp01 (0.5 +      0.5*r - 0.418688*g - 0.081312*b)
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr RGBA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr HSI e where
  toPixelYCbCr = toPixelYCbCr . toPixelRGB
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr HSIA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr YCbCr e where
  toPixelYCbCr = toDouble
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr YCbCrA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr CMYK e where
  toPixelYCbCr = toPixelYCbCr . toPixelRGB
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr CMYKA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}


-- ToYCbCrA


-- | Conversion to `YCbCrA` from another color space with Alpha channel.
class ToYCbCr cs e => ToYCbCrA cs e where

  -- | Convert to an `YCbCrA` pixel.
  toPixelYCbCrA :: Pixel cs e -> Pixel YCbCrA Double
  toPixelYCbCrA = addAlpha 1 . toPixelYCbCr
  {-# INLINE toPixelYCbCrA #-}


instance ToYCbCr Y e => ToYCbCrA Y e

instance Elevator e => ToYCbCrA YA e where
  toPixelYCbCrA !px = addAlpha (eToDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance ToYCbCr RGB e => ToYCbCrA RGB e


instance ToYCbCr HSI e => ToYCbCrA HSI e

instance Elevator e => ToYCbCrA HSIA e where
  toPixelYCbCrA !px = addAlpha (eToDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance Elevator e => ToYCbCrA RGBA e where
  toPixelYCbCrA !px = addAlpha (eToDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance ToYCbCr CMYK e => ToYCbCrA CMYK e

instance Elevator e => ToYCbCrA CMYKA e where
  toPixelYCbCrA !px = addAlpha (eToDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance ToYCbCr YCbCr e => ToYCbCrA YCbCr e

instance Elevator e => ToYCbCrA YCbCrA e where
  toPixelYCbCrA = toDouble
  {-# INLINE toPixelYCbCrA #-}


-- | Change pixel precision to `Word8`.
toWord8 :: (Functor (Pixel cs), Elevator e) => Pixel cs e -> Pixel cs Word8
toWord8 = fmap eToWord8
{-# INLINABLE toWord8 #-}


-- | Change pixel precision to `Word16`.
toWord16 :: (Functor (Pixel cs), Elevator e) => Pixel cs e -> Pixel cs Word16
toWord16 = fmap eToWord16
{-# INLINABLE toWord16 #-}


-- | Change pixel precision to `Word32`.
toWord32 :: (Functor (Pixel cs), Elevator e) => Pixel cs e -> Pixel cs Word32
toWord32 = fmap eToWord32
{-# INLINABLE toWord32 #-}


-- | Change pixel precision to `Word64`.
toWord64 :: (Functor (Pixel cs), Elevator e) => Pixel cs e -> Pixel cs Word64
toWord64 = fmap eToWord64
{-# INLINABLE toWord64 #-}


-- | Change pixel precision to `Float`.
toFloat :: (Functor (Pixel cs), Elevator e) => Pixel cs e -> Pixel cs Float
toFloat = fmap eToFloat
{-# INLINABLE toFloat #-}


-- | Change pixel precision to `Double`.
toDouble :: (Functor (Pixel cs), Elevator e) => Pixel cs e -> Pixel cs Double
toDouble = fmap eToDouble
{-# INLINABLE toDouble #-}



-- | Change pixel precision from `Double`.
fromDouble :: (Functor (Pixel cs), Elevator e) => Pixel cs Double -> Pixel cs e
fromDouble = fmap eFromDouble
{-# INLINABLE fromDouble #-}
