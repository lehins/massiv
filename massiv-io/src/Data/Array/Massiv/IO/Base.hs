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
module Data.Array.Massiv.IO.Base
  ( FileFormat(..)
  , Readable(..)
  , Writable(..)
  , Sequence(..)
  , Auto(..)
  , Image
  , defaultReadOptions
  , defaultWriteOptions
  ) where

import           Data.Array.Massiv    (Array, DIM2)
import qualified Data.ByteString      as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Default         (Default (..))

import           Graphics.ColorSpace  (Pixel)

type Image r cs e = Array r DIM2 (Pixel cs e)


-- data UnknownExtension

-- data UnsupportedArrayType

-- | Generate default read options for a file format
defaultReadOptions :: FileFormat f => f -> ReadOptions f
defaultReadOptions _ = def


-- | Generate default write options for a file format
defaultWriteOptions :: FileFormat f => f -> WriteOptions f
defaultWriteOptions _ = def


-- | Special wrapper for formats that support encoding/decoding sequence of array.
newtype Sequence f = Sequence f

newtype Auto f = Auto f

-- | File format. Helps in guessing file format from a file extension,
-- as well as supplying format specific options during saving the file.
class (Default (ReadOptions f), Default (WriteOptions f)) => FileFormat f where
  -- | Options that can be used during reading a file in this format.
  type ReadOptions f
  type ReadOptions f = ()

  -- | Options that can be used during writing a file in this format.
  type WriteOptions f
  type WriteOptions f = ()

  -- | Default file extension for this file format.
  ext :: f -> String

  -- | Other known file extensions for this file format, eg. ".jpeg", ".jpg".
  exts :: f -> [String]
  exts f = [ext f]

  -- | Checks if a file extension corresponds to the format, eg.
  -- @isFormat ".png" PNG == True@
  isFormat :: String -> f -> Bool
  isFormat e f = e `elem` exts f


instance FileFormat f => FileFormat (Auto f) where
  type ReadOptions (Auto f) = ReadOptions f
  type WriteOptions (Auto f) = WriteOptions f

  ext (Auto f) = ext f

-- | File formats that can be read into an Array.
class Readable f arr where

  -- | Decode a `B.ByteString` into an Array.
  decode :: f -> ReadOptions f -> B.ByteString -> Either String arr


-- | Arrays that can be written into a file.
class Writable f arr where

  -- | Encode an array into a `BL.ByteString`.
  encode :: f -> WriteOptions f -> arr -> Either String BL.ByteString

