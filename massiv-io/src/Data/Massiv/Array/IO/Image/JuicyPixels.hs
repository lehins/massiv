-- |
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels
  ( -- * JuicyPixels formats
    -- ** BMP
    module BMP
    -- ** GIF
  , module GIF
    -- ** HDR
  , module HDR
    -- ** JPG
  , module JPG
    -- ** PNG
  , module PNG
    -- ** TGA
  , module TGA
    -- ** TIF
  , module TIF
    -- ** JuicyPixels conversion
  , toJPImageY8
  , toJPImageY16
  , toJPImageY32
  , toJPImageYA8
  , toJPImageYA16
  , toJPImageYF
  , toJPImageRGB8
  , toJPImageRGB16
  , toJPImageRGBA8
  , toJPImageRGBA16
  , toJPImageRGBF
  , toJPImageYCbCr8
  , toJPImageCMYK8
  , toJPImageCMYK16
  , fromDynamicImage
  , fromDynamicImageAuto
  ) where

import Data.Massiv.Array.IO.Image.JuicyPixels.Base
import Data.Massiv.Array.IO.Image.JuicyPixels.BMP as BMP
import Data.Massiv.Array.IO.Image.JuicyPixels.GIF as GIF
import Data.Massiv.Array.IO.Image.JuicyPixels.HDR as HDR
import Data.Massiv.Array.IO.Image.JuicyPixels.JPG as JPG
import Data.Massiv.Array.IO.Image.JuicyPixels.PNG as PNG
import Data.Massiv.Array.IO.Image.JuicyPixels.TGA as TGA
import Data.Massiv.Array.IO.Image.JuicyPixels.TIF as TIF
