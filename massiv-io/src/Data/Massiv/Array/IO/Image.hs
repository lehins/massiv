{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image
  ( Encode
  , encodeImage
  , imageWriteFormats
  , imageWriteAutoFormats
  , Decode
  , decodeImage
  , imageReadFormats
  , imageReadAutoFormats
  , module Data.Massiv.Array.IO.Image.JuicyPixels
  , module Data.Massiv.Array.IO.Image.Netpbm
  ) where

import Control.Exception (throw)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Char (toLower)
import Data.Massiv.Array
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.IO.Image.JuicyPixels
import Data.Massiv.Array.IO.Image.Netpbm
import Graphics.ColorSpace
import Prelude as P
import System.FilePath (takeExtension)



data Encode out where
  EncodeAs :: (FileFormat f, Writable f out) => f -> Encode out

instance Show (Encode out) where
  show (EncodeAs f) = show f

instance FileFormat (Encode (Image r cs e)) where
  ext (EncodeAs f) = ext f

  exts (EncodeAs f) = exts f

instance Writable (Encode (Image r cs e)) (Image r cs e) where
  encode (EncodeAs f) _ = encode f (defaultWriteOptions f)

-- | Encode an image into a lazy `BL.ByteString`, while selecting the appropriate format from the
-- file extension.
encodeImage
  :: (Source r Ix2 (Pixel cs e), ColorSpace cs e)
  => [Encode (Image r cs e)] -- ^ List of image formats to choose from (useful lists are
                             -- `imageWriteFormats` and `imageWriteAutoFormats`
  -> FilePath -- ^ File name with extension, so the format can be inferred
  -> Image r cs e -- ^ Image to encode
  -> BL.ByteString
encodeImage formats path img = do
  let ext' = P.map toLower . takeExtension $ path
  case dropWhile (not . isFormat ext') formats of
    []    -> throw $ EncodeError $ "File format is not supported: " ++ ext'
    (f:_) -> encode f () img

-- | List of image formats that can be encoded without any color space conversion.
imageWriteFormats :: (Source r Ix2 (Pixel cs e), ColorSpace cs e) => [Encode (Image r cs e)]
imageWriteFormats =
  [ EncodeAs BMP
  , EncodeAs GIF
  , EncodeAs HDR
  , EncodeAs JPG
  , EncodeAs PNG
  , EncodeAs TGA
  , EncodeAs TIF
  ]

-- | List of image formats that can be encoded with any necessary color space conversions.
imageWriteAutoFormats
  :: ( Source r Ix2 (Pixel cs e)
     , ColorSpace cs e
     , ToYA cs e
     , ToRGBA cs e
     , ToYCbCr cs e
     , ToCMYK cs e
     )
  => [Encode (Image r cs e)]
imageWriteAutoFormats =
  [ EncodeAs (Auto BMP)
  , EncodeAs (Auto GIF)
  , EncodeAs (Auto HDR)
  , EncodeAs (Auto JPG)
  , EncodeAs (Auto PNG)
  , EncodeAs (Auto TGA)
  , EncodeAs (Auto TIF)
  ]



data Decode out where
  DecodeAs :: (FileFormat f, Readable f out) => f -> Decode out

instance Show (Decode out) where
  show (DecodeAs f) = show f

instance FileFormat (Decode (Image r cs e)) where
  ext (DecodeAs f) = ext f

  exts (DecodeAs f) = exts f

instance Readable (Decode (Image r cs e)) (Image r cs e) where
  decode (DecodeAs f) _ = decode f (defaultReadOptions f)

-- | Decode an image from the strict `ByteString` while inferring format the image is encoded in
-- from the file extension
decodeImage
  :: (Source r Ix2 (Pixel cs e), ColorSpace cs e)
  => [Decode (Image r cs e)] -- ^ List of available formats to choose from
  -> FilePath -- ^ File name with extension, so format can be inferred
  -> B.ByteString -- ^ Encoded image
  -> Image r cs e
decodeImage formats path bs = do
  let ext' = P.map toLower . takeExtension $ path
  case dropWhile (not . isFormat ext') formats of
    []    -> throw $ DecodeError $ "File format is not supported: " ++ ext'
    (f:_) -> decode f () bs

-- | List of image formats decodable with no colorspace conversion
imageReadFormats
  :: (Source S Ix2 (Pixel cs e), ColorSpace cs e)
  => [Decode (Image S cs e)]
imageReadFormats =
  [ DecodeAs BMP
  , DecodeAs GIF
  , DecodeAs HDR
  , DecodeAs JPG
  , DecodeAs PNG
  , DecodeAs TGA
  , DecodeAs TIF
  , DecodeAs PBM
  , DecodeAs PGM
  , DecodeAs PPM
  ]

-- | List of image formats decodable with no colorspace conversion
imageReadAutoFormats
  :: (Mutable r Ix2 (Pixel cs e), ColorSpace cs e)
  => [Decode (Image r cs e)]
imageReadAutoFormats =
  [ DecodeAs (Auto BMP)
  , DecodeAs (Auto GIF)
  , DecodeAs (Auto HDR)
  , DecodeAs (Auto JPG)
  , DecodeAs (Auto PNG)
  , DecodeAs (Auto TGA)
  , DecodeAs (Auto TIF)
  , DecodeAs (Auto PBM)
  , DecodeAs (Auto PGM)
  , DecodeAs (Auto PPM)
  ]
