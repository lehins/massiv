{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image
  ( module Data.Massiv.Array.IO.Image.JuicyPixels
  , module Data.Massiv.Array.IO.Image.Netpbm
  -- ** Helper image functions
  , Encode
  , encodeImageM
  , imageWriteFormats
  , imageWriteAutoFormats
  , Decode
  , decodeImageM
  , imageReadFormats
  , imageReadAutoFormats
  ) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Char (toLower)
import Data.Massiv.Array
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.IO.Image.JuicyPixels
import Data.Massiv.Array.IO.Image.Netpbm
import Graphics.Pixel.ColorSpace
import Prelude as P
import System.FilePath (takeExtension)



data Encode out where
  EncodeAs
    :: FileFormat f
    => f
    -> (forall m. MonadThrow m =>
                    f -> out -> m BL.ByteString)
    -> Encode out

instance Show (Encode out) where
  show (EncodeAs f _) = show f

instance FileFormat (Encode (Image r cs e)) where
  ext (EncodeAs f _) = ext f

  exts (EncodeAs f _) = exts f

instance Writable (Encode (Image r cs e)) (Image r cs e) where
  encodeM (EncodeAs f enc) _ = enc f


-- | Encode an image into a lazy `BL.ByteString`, while selecting the appropriate format from the
-- file extension.
--
-- @since 0.2.0
encodeImageM
  :: MonadThrow m
  => [Encode (Image r cs e)] -- ^ List of image formats to choose from (useful lists are
                             -- `imageWriteFormats` and `imageWriteAutoFormats`
  -> FilePath -- ^ File name with extension, so the format can be inferred
  -> Image r cs e -- ^ Image to encode
  -> m BL.ByteString
encodeImageM formats path img = do
  let ext' = P.map toLower . takeExtension $ path
  case dropWhile (not . isFormat ext') formats of
    []    -> throwM $ EncodeError $ "File format is not supported: " ++ ext'
    (f:_) -> encodeM f () img



-- | List of image formats that can be encoded without any color space conversion.
imageWriteFormats :: (Source r Ix2 (Pixel cs e), ColorModel cs e) => [Encode (Image r cs e)]
imageWriteFormats =
  [ EncodeAs BMP (`encodeBMP` def)
  , EncodeAs GIF (`encodeGIF` def)
  , EncodeAs HDR (`encodeHDR` def)
  , EncodeAs JPG (`encodeJPG` def)
  , EncodeAs PNG encodePNG
  , EncodeAs TGA encodeTGA
  , EncodeAs TIF encodeTIF
  ]

-- | List of image formats that can be encoded with any necessary color space conversions.
imageWriteAutoFormats
  :: ( Source r Ix2 (Pixel cs e)
     , ColorSpace cs i e
     , ColorSpace (BaseSpace cs) i e
     )
  => [Encode (Image r cs e)]
imageWriteAutoFormats =
  [ EncodeAs (Auto BMP) (\(Auto BMP) -> pure . encodeAutoBMP def)
  , EncodeAs (Auto GIF) (\(Auto GIF) -> encodeAutoGIF def)
  , EncodeAs (Auto HDR) (\(Auto HDR) -> pure . encodeAutoHDR def)
  , EncodeAs (Auto JPG) (\(Auto JPG) -> pure . encodeAutoJPG def)
  , EncodeAs (Auto PNG) (\(Auto PNG) -> pure . encodeAutoPNG)
  , EncodeAs (Auto TGA) (\(Auto TGA) -> pure . encodeAutoTGA)
  , EncodeAs (Auto TIF) (\(Auto TIF) -> pure . encodeAutoTIF)
  ]



data Decode out where
  DecodeAs
    :: FileFormat f
    => f
    -> (forall m. MonadThrow m =>
                    f -> B.ByteString -> m out)
    -> Decode out

instance Show (Decode out) where
  show (DecodeAs f _) = show f

instance FileFormat (Decode (Image r cs e)) where
  ext (DecodeAs f _) = ext f

  exts (DecodeAs f _) = exts f

instance Readable (Decode (Image r cs e)) (Image r cs e) where
  decodeM (DecodeAs f dec) = dec f


-- | Decode an image from the strict `ByteString` while inferring format the image is encoded in
-- from the file extension
--
-- @since 0.2.0
decodeImageM
  :: MonadThrow m
  => [Decode (Image r cs e)] -- ^ List of available formats to choose from
  -> FilePath -- ^ File name with extension, so format can be inferred
  -> B.ByteString -- ^ Encoded image
  -> m (Image r cs e)
decodeImageM formats path bs = do
  let ext' = P.map toLower . takeExtension $ path
  case dropWhile (not . isFormat ext') formats of
    []    -> throwM $ DecodeError $ "File format is not supported: " ++ ext'
    (f:_) -> decodeM f bs

-- | List of image formats decodable with no color space conversion
imageReadFormats :: ColorModel cs e => [Decode (Image S cs e)]
imageReadFormats =
  [ DecodeAs BMP decodeBMP
  , DecodeAs GIF decodeGIF
  , DecodeAs HDR decodeHDR
  , DecodeAs JPG decodeJPG
  , DecodeAs PNG decodePNG
  , DecodeAs TGA decodeTGA
  , DecodeAs TIF decodeTIF
  , DecodeAs PBM (\f -> fmap fst . decodeNetpbmImage f)
  , DecodeAs PGM (\f -> fmap fst . decodeNetpbmImage f)
  , DecodeAs PPM (\f -> fmap fst . decodeNetpbmImage f)
  ]

-- | List of image formats decodable with automatic colorspace conversion
imageReadAutoFormats
  :: (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e)
  => [Decode (Image r cs e)]
imageReadAutoFormats =
  [ DecodeAs (Auto BMP) decodeAutoBMP
  , DecodeAs (Auto GIF) decodeAutoGIF
  , DecodeAs (Auto HDR) decodeAutoHDR
  , DecodeAs (Auto JPG) decodeAutoJPG
  , DecodeAs (Auto PNG) decodeAutoPNG
  , DecodeAs (Auto TGA) decodeAutoTGA
  , DecodeAs (Auto TIF) decodeAutoTIF
  , DecodeAs (Auto PBM) (\f -> fmap fst . decodeAutoNetpbmImage f)
  , DecodeAs (Auto PGM) (\f -> fmap fst . decodeAutoNetpbmImage f)
  , DecodeAs (Auto PPM) (\f -> fmap fst . decodeAutoNetpbmImage f)
  ]


