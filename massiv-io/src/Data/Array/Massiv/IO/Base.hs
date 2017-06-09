{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.IO.Base
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.IO.Base (
  FileFormat(..), Readable(..), Writable(..), Sequence(..), Image
  ) where

import           Data.Array.Massiv
import qualified Data.ByteString      as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Typeable        (Typeable)
import           Graphics.ColorSpace

type Image r cs e = Array r DIM2 (Pixel cs e)


-- data UnknownExtension

-- data UnsupportedArrayType



-- | Special wrapper for formats that support encoding/decoding sequence of array.
newtype Sequence f = Sequence f

-- | File format. Helps in guessing file format from a file extension,
-- as well as supplying format specific options during saving the file.
class Typeable format => FileFormat format where
  -- | Options that can be used during reading a file in this format.
  type ReadOptions format
  type ReadOptions format = ()

  -- | Options that can be used during writing a file in this format.
  type WriteOptions format
  type WriteOptions format = ()

  -- | Default file extension for this file format.
  ext :: format -> String

  -- | Other known file extensions for this file format, eg. ".jpeg", ".jpg".
  exts :: format -> [String]
  exts f = [ext f]

  -- | Checks if a file extension corresponds to the format, eg.
  -- @isFormat ".png" PNG == True@
  isFormat :: String -> format -> Bool
  isFormat e f = e `elem` exts f


-- | File formats that can be read into an Array.
class FileFormat format => Readable format arr where

  -- | Decode a `B.ByteString` into an Array.
  decode :: format -> ReadOptions format -> B.ByteString -> Either String arr

  -- | Decode a `B.ByteString` into an Array.
  keenDecode :: format -> ReadOptions format -> B.ByteString -> Either String arr


-- | Arrays that can be written into a file.
class FileFormat format => Writable format arr where

  -- | Encode an array into a `BL.ByteString`.
  encode :: format -> WriteOptions format -> arr -> Either String BL.ByteString

  -- | Encode an array into a `BL.ByteString`.
  keenEncode :: format -> WriteOptions format -> arr -> Either String BL.ByteString
