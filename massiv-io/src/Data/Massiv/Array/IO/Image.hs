{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
  , encodeImageM
  , imageWriteFormats
  , imageWriteAutoFormats
  , Decode
  , decodeImage
  , decodeImageM
  , imageReadFormats
  , imageReadAutoFormats
  , module Data.Massiv.Array.IO.Image.JuicyPixels.TIF
  , module Data.Massiv.Array.IO.Image.Netpbm
  ) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Char (toLower)
import Data.Massiv.Array
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.IO.Image.JuicyPixels.TIF
import Data.Massiv.Array.IO.Image.Netpbm
import Graphics.Color.Model
import Graphics.Color.Pixel
import Prelude as P
import System.FilePath (takeExtension)



data Encode out where
  EncodeAs
    :: (FileFormat f)
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


-- | Encode an array into a `BL.ByteString`.
encodeImage ::
     (ColorModel cs e, Source r Ix2 (Pixel cs e))
  => [Encode (Image r cs e)]
  -> FilePath
  -> Image r cs e
  -> BL.ByteString
encodeImage formats path = either throw id . encodeImageM formats path
{-# DEPRECATED encodeImage "In favor of a better `encodeImageM`" #-}

-- | Decode a `B.ByteString` into an Array.
decodeImage ::
     (ColorModel cs e, Source r Ix2 (Pixel cs e))
  => [Decode (Image r cs e)]
  -> FilePath
  -> B.ByteString
  -> Image r cs e
decodeImage formats path = either throw fst . decodeImageM formats path
{-# DEPRECATED decodeImage "In favor of a better `decodeImageM`" #-}


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
  -- [ EncodeAs BMP
  -- , EncodeAs GIF
  -- , EncodeAs HDR
  -- , EncodeAs JPG
  -- , EncodeAs PNG
  -- , EncodeAs TGA
  [ EncodeAs TIF (const encodeTIF)
  ]

-- | List of image formats that can be encoded with any necessary color space conversions.
imageWriteAutoFormats
  :: ( Source r Ix2 (Pixel cs e)
     , ColorSpace cs i e
     , ColorSpace (BaseSpace cs) i e
     )
  => [Encode (Image r cs e)]
imageWriteAutoFormats =
  -- [ EncodeAs (Auto BMP)
  -- , EncodeAs (Auto GIF)
  -- , EncodeAs (Auto HDR)
  -- , EncodeAs (Auto JPG)
  -- , EncodeAs (Auto PNG)
  -- , EncodeAs (Auto TGA)
  [ EncodeAs (Auto TIF) (\_ -> pure . encodeAutoTIF)
  ]



data Decode out where
  DecodeAs
    :: (FileFormat f)
    => f
    -> (forall m. MonadThrow m =>
                    f -> B.ByteString -> m (out, Maybe B.ByteString))
    -> Decode out

instance Show (Decode out) where
  show (DecodeAs f _) = show f

instance FileFormat (Decode (Image r cs e)) where
  ext (DecodeAs f _) = ext f

  exts (DecodeAs f _) = exts f

instance Readable (Decode (Image r cs e)) (Image r cs e) where
  decodeM (DecodeAs f dec) _ = dec f


-- | Decode an image from the strict `ByteString` while inferring format the image is encoded in
-- from the file extension
--
-- @since 0.2.0
decodeImageM
  :: MonadThrow m
  => [Decode (Image r cs e)] -- ^ List of available formats to choose from
  -> FilePath -- ^ File name with extension, so format can be inferred
  -> B.ByteString -- ^ Encoded image
  -> m (Image r cs e, Maybe B.ByteString)
decodeImageM formats path bs = do
  let ext' = P.map toLower . takeExtension $ path
  case dropWhile (not . isFormat ext') formats of
    []    -> throwM $ DecodeError $ "File format is not supported: " ++ ext'
    (f:_) -> decodeM f () bs

-- | List of image formats decodable with no color space conversion
imageReadFormats :: ColorModel cs e => [Decode (Image S cs e)]
imageReadFormats =
  -- [ DecodeAs BMP
  -- , DecodeAs GIF
  -- , DecodeAs HDR
  -- , DecodeAs JPG
  -- , DecodeAs PNG
  -- , DecodeAs TGA
  [
    DecodeAs TIF (\f bs -> (, Nothing) <$> decodeTIF f bs)
  , DecodeAs PBM decodeNetpbmImage
  , DecodeAs PGM decodeNetpbmImage
  , DecodeAs PPM decodeNetpbmImage
  ]

-- | List of image formats decodable with automatic colorspace conversion
imageReadAutoFormats
  :: (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e)
  => [Decode (Image r cs e)]
imageReadAutoFormats =
  -- [ DecodeAs (Auto BMP)
  -- , DecodeAs (Auto GIF)
  -- , DecodeAs (Auto HDR)
  -- , DecodeAs (Auto JPG)
  -- , DecodeAs (Auto PNG)
  -- , DecodeAs (Auto TGA)
  [ DecodeAs (Auto TIF) (\f bs -> (, Nothing) <$> decodeAutoTIF f bs)
  , DecodeAs (Auto PBM) decodeAutoNetpbmImage
  , DecodeAs (Auto PGM) decodeAutoNetpbmImage
  , DecodeAs (Auto PPM) decodeAutoNetpbmImage
  ]


