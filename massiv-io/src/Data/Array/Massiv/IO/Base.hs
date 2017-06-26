{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
  , ConvertError(..)
  , EncodeError(..)
  , DecodeError(..)
  , Sequence(..)
  , Auto(..)
  , Image
  , defaultReadOptions
  , defaultWriteOptions
  , toProxy
  , fromMaybeEncode
  , fromEitherDecode
  ) where

import           Control.Exception
import           Data.Array.Massiv    (Array, DIM2, S)
import qualified Data.ByteString      as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Default         (Default (..))
import           Data.Maybe           (fromMaybe)
import           Data.Typeable
import           Graphics.ColorSpace  (ColorSpace, Pixel)

type Image r cs e = Array r DIM2 (Pixel cs e)

-- | Conversion error, which is thrown when there is a mismatch between the
-- expected array type and the one supported by the file format. It is also
-- thrown upon a failure of automatic conversion between those types, in case
-- such conversion is utilized.
newtype ConvertError = ConvertError String deriving Show

instance Exception ConvertError

-- | This exception can be thrown while reading/decoding a file and indicates an
-- error in the file itself.
newtype DecodeError = DecodeError String deriving Show

instance Exception DecodeError

-- | This exception can be thrown while writing/encoding into a file and
-- indicates an error in an array that is being encoded.
newtype EncodeError = EncodeError String deriving Show

instance Exception EncodeError


-- | Generate default read options for a file format
defaultReadOptions :: FileFormat f => f -> ReadOptions f
defaultReadOptions _ = def


-- | Generate default write options for a file format
defaultWriteOptions :: FileFormat f => f -> WriteOptions f
defaultWriteOptions _ = def


-- | Special wrapper for formats that support encoding/decoding sequence of array.
newtype Sequence f = Sequence f deriving Show

newtype Auto f = Auto f deriving Show

-- | File format. Helps in guessing file format from a file extension,
-- as well as supplying format specific options during saving the file.
class (Default (ReadOptions f), Default (WriteOptions f), Show f) => FileFormat f where
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
  decode :: f -> ReadOptions f -> B.ByteString -> arr


-- | Arrays that can be written into a file.
class Writable f arr where

  -- | Encode an array into a `BL.ByteString`.
  encode :: f -> WriteOptions f -> arr -> BL.ByteString


-- | Helper function to create a `Proxy` from the value.
toProxy :: a -> Proxy a
toProxy _ = Proxy

-- | Encode an image using the supplied function or throw an error in case of failure.
fromMaybeEncode
  :: forall f r cs e b. (ColorSpace cs e, FileFormat f, Typeable r)
  => f -> Proxy (Image r cs e) -> Maybe b -> b
fromMaybeEncode _ _         (Just b) = b
fromMaybeEncode f _imgProxy Nothing =
  throw $
  ConvertError
    ("Format " ++
     show f ++
     " cannot be encoded as <Image " ++
     showsTypeRep (typeRep (Proxy :: Proxy r)) " " ++
     showsTypeRep (typeRep (Proxy :: Proxy cs)) " " ++
     showsTypeRep (typeRep (Proxy :: Proxy e)) ">")


-- | Decode an image using the supplied function or throw an error in case of failure.
fromEitherDecode :: forall cs e a f. (ColorSpace cs e, FileFormat f) =>
                    f
                 -> (a -> String)
                 -> (a -> Maybe (Image S cs e))
                 -> Either String a
                 -> Image S cs e
fromEitherDecode _ _      _    (Left err)   = throw $ DecodeError err
fromEitherDecode f showCS conv (Right eImg) =
  fromMaybe
    (throw $
     ConvertError
       ("Cannot decode " ++ show f ++ " image <" ++
        showCS eImg ++
        "> as " ++
        "<Image S " ++
        showsTypeRep (typeRep (Proxy :: Proxy cs)) " " ++
        showsTypeRep (typeRep (Proxy :: Proxy e)) ">"))
    (conv eImg)
