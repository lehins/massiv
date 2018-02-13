{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image
-- Copyright   : (c) Alexey Kuleshevich 2018
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

import           Control.Exception                      (throw)
import           Data.Massiv.Array
import           Data.Massiv.Array.IO.Base
import           Data.Massiv.Array.IO.Image.JuicyPixels
import           Data.Massiv.Array.IO.Image.Netpbm
import qualified Data.ByteString                        as B (ByteString)
import qualified Data.ByteString.Lazy                   as BL (ByteString)
import           Data.Char                              (toLower)
import           Graphics.ColorSpace
import           Prelude                                as P
import           System.FilePath                        (takeExtension)



data Encode out where
  EncodeAs :: (FileFormat f, Writable f out) => f -> Encode out

instance Show (Encode out) where
  show (EncodeAs f) = show f

instance FileFormat (Encode (Image r cs e)) where
  ext (EncodeAs f) = ext f

instance Writable (Encode (Image r cs e)) (Image r cs e) where
  encode (EncodeAs f) _ = encode f (defaultWriteOptions f)


encodeImage
  :: (Source r Ix2 (Pixel cs e), ColorSpace cs e)
  => [Encode (Image r cs e)]
  -> FilePath
  -> Image r cs e
  -> BL.ByteString
encodeImage formats path img = do
  let ext' = P.map toLower . takeExtension $ path
  case dropWhile (not . isFormat ext') formats of
    []    -> throw $ EncodeError $ "File format is not supported: " ++ ext'
    (f:_) -> encode f () img


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

instance Readable (Decode (Image r cs e)) (Image r cs e) where
  decode (DecodeAs f) _ = decode f (defaultReadOptions f)


decodeImage
  :: (Source r Ix2 (Pixel cs e), ColorSpace cs e)
  => [Decode (Image r cs e)]
  -> FilePath
  -> B.ByteString
  -> Image r cs e
decodeImage formats path bs = do
  let ext' = P.map toLower . takeExtension $ path
  case dropWhile (not . isFormat ext') formats of
    []    -> throw $ DecodeError $ "File format is not supported: " ++ ext'
    (f:_) -> decode f () bs


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

imageReadAutoFormats
  :: (Target r Ix2 (Pixel cs e), ColorSpace cs e)
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
