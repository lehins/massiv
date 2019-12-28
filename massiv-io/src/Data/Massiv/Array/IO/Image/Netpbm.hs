{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image.Netpbm
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.Netpbm
  ( -- * Netpbm formats
    -- ** PBM
    PBM(..)
    -- ** PGM
  , PGM(..)
    -- ** PPM
  , PPM(..)
  , decodeNetpbmImage
  , decodeNetpbmImageAuto
  , decodeNetpbmImageSequence
  , decodeNetpbmImageSequenceAuto
  ) where

import Control.Monad (guard)
import qualified Data.ByteString as B (ByteString)
import Data.Massiv.Array as M
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.Manifest.Vector
import Data.Typeable
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Graphics.Color.Algebra.Binary
import Graphics.Color.Pixel
import qualified Graphics.Color.Model.RGB as CM
import qualified Graphics.Color.Model.Y as CM
import Graphics.Netpbm as Netpbm hiding (PPM)
import qualified Graphics.Netpbm as Netpbm (PPM(..))
import Prelude as P

#if !MIN_VERSION_massiv(0, 2, 7)
pattern Sz :: ix -> ix
pattern Sz ix = ix
type Sz ix = ix
#endif
#if !MIN_VERSION_massiv(0, 3, 0)
fromVectorM ::
     (Construct S ix a, Mutable S ix a, Storable a)
  => Comp
  -> Sz ix
  -> V.Vector a
  -> Maybe (Array S ix a)
fromVectorM comp sz = pure . fromVector comp sz
#endif

-- | Netpbm: portable bitmap image with @.pbm@ extension.
data PBM = PBM deriving Show

instance FileFormat PBM where
  ext _ = ".pbm"

instance FileFormat (Sequence PBM) where
  type WriteOptions (Sequence PBM) = WriteOptions PBM
  ext _ = ext PBM

instance FileFormat (Sequence (Auto PBM)) where
  type WriteOptions (Sequence (Auto PBM)) = WriteOptions PBM
  ext _ = ext PBM


-- | Netpbm: portable graymap image with @.pgm@ extension.
data PGM = PGM deriving Show

instance FileFormat PGM where
  ext _ = ".pgm"

instance FileFormat (Sequence PGM) where
  type WriteOptions (Sequence PGM) = WriteOptions PGM
  ext _ = ext PGM

instance FileFormat (Sequence (Auto PGM)) where
  type WriteOptions (Sequence (Auto PGM)) = WriteOptions PGM
  ext _ = ext PGM


-- | Netpbm: portable pixmap image with @.ppm@ extension.
data PPM = PPM deriving Show

instance FileFormat PPM where
  ext _ = ".ppm"
  exts _ = [".ppm", ".pnm"]

instance FileFormat (Sequence PPM) where
  type WriteOptions (Sequence PPM) = WriteOptions PPM
  ext _ = ext PPM

instance FileFormat (Sequence (Auto PPM)) where
  type WriteOptions (Sequence (Auto PPM)) = WriteOptions PPM
  ext _ = ext PPM

-- | Try to decode a Netpbm image
--
-- @since 0.2.0
decodeNetpbmImage ::
     (FileFormat f, ColorModel cs e, MonadThrow m)
  => f
  -> B.ByteString
  -> m (Image S cs e, Maybe B.ByteString)
decodeNetpbmImage f = decodePPM f fromNetpbmImage

-- | Try to decode a Netpbm image sequence
--
-- @since 0.2.0
decodeNetpbmImageSequence ::
     (FileFormat f, ColorModel cs e, MonadThrow m)
  => f
  -> B.ByteString
  -> m (Array B Ix1 (Image S cs e), Maybe B.ByteString)
decodeNetpbmImageSequence f = decodePPMs f fromNetpbmImage

-- | Try to decode a Netpbm image, while auto converting the colorspace.
--
-- @since 0.2.0
decodeNetpbmImageAuto ::
     (FileFormat f, Mutable r Ix2 (Pixel cs e), MonadThrow m, ColorSpace cs i e)
  => f
  -> B.ByteString
  -> m (Image r cs e, Maybe B.ByteString)
decodeNetpbmImageAuto f = decodePPM f fromNetpbmImageAuto

-- | Try to decode a Netpbm image sequence, while auto converting the colorspace.
--
-- @since 0.2.0
decodeNetpbmImageSequenceAuto ::
     (FileFormat f, Mutable r Ix2 (Pixel cs e), MonadThrow m, ColorSpace cs i e)
  => f
  -> B.ByteString
  -> m (Array B Ix1 (Image r cs e), Maybe B.ByteString)
decodeNetpbmImageSequenceAuto f = decodePPMs f fromNetpbmImageAuto

decodePPMs :: (FileFormat f, Mutable r Ix2 (Pixel cs e), ColorModel cs e, MonadThrow m) =>
              f
           -> (Netpbm.PPM -> Maybe (Image r cs e))
           -> B.ByteString
           -> m (Array B Ix1 (Image r cs e), Maybe B.ByteString)
decodePPMs f converter bs =
  case parsePPM bs of
    Left err -> throwM $ DecodeError err
    Right ([], Nothing) -> throwM $ DecodeError "PNM image is empty"
    Right ([], _) -> throwM $ DecodeError "Cannot parse PNM image"
    Right (ppms, leftOver) -> do
      imgs <- P.traverse (fromEitherDecode f showNetpbmCS converter) ppms
      pure (fromList Seq imgs, leftOver)
{-# INLINE decodePPMs #-}


decodePPM :: (FileFormat f, Mutable r Ix2 (Pixel cs e), ColorModel cs e, MonadThrow m) =>
             f
          -> (Netpbm.PPM -> Maybe (Image r cs e))
          -> B.ByteString
          -> m (Image r cs e, Maybe B.ByteString)
decodePPM f decoder bs =
  case parsePPM bs of
    Left err -> throwM $ DecodeError err
    Right ([], Nothing) -> throwM $ DecodeError "PNM image is empty"
    Right ([], _) -> throwM $ DecodeError "Cannot parse PNM image"
    Right (ppm:_, leftover) -> do
      img <- fromEitherDecode f showNetpbmCS decoder ppm
      pure (img, leftover)
{-# INLINE decodePPM #-}


fromNetpbmImageUnsafe
  :: (Storable a, Storable (Pixel cs e))
  => Int -> Int -> V.Vector a -> Maybe (Image S cs e)
fromNetpbmImageUnsafe m n v = do
  guard (n * m == V.length v)
  fromVectorM Par (Sz (m :. n)) $ V.unsafeCast v



showNetpbmCS :: Netpbm.PPM -> String
showNetpbmCS Netpbm.PPM {ppmData} =
  case ppmData of
    PbmPixelData _      -> "Image S Y Bit"
    PgmPixelData8 _     -> "Image S Y Word8"
    PgmPixelData16 _    -> "Image S Y Word16"
    PpmPixelDataRGB8 _  -> "Image S RGB Word8"
    PpmPixelDataRGB16 _ -> "Image S RGB Word16"



instance Readable PBM (Image S CM.Y Bit) where
  decodeM f _ = decodeNetpbmImage f
instance Readable (Sequence PBM) (Array B Ix1 (Image S CM.Y Bit)) where
  decodeM f _ = decodePPMs f fromNetpbmImage

instance Readable PGM (Image S CM.Y Word8) where
  decodeM f _ = decodeNetpbmImage f
instance Readable PGM (Image S CM.Y Word16) where
  decodeM f _ = decodeNetpbmImage f
instance Readable (Sequence PGM) (Array B Ix1 (Image S CM.Y Word8)) where
  decodeM f _ = decodePPMs f fromNetpbmImage
instance Readable (Sequence PGM) (Array B Ix1 (Image S CM.Y Word16)) where
  decodeM f _ = decodePPMs f fromNetpbmImage

instance Readable PPM (Image S CM.RGB Word8) where
  decodeM f _ = decodeNetpbmImage f
instance Readable PPM (Image S CM.RGB Word16) where
  decodeM f _ = decodeNetpbmImage f
instance Readable (Sequence PPM) (Array B Ix1 (Image S CM.RGB Word8)) where
  decodeM f _ = decodePPMs f fromNetpbmImage
instance Readable (Sequence PPM) (Array B Ix1 (Image S CM.RGB Word16)) where
  decodeM f _ = decodePPMs f fromNetpbmImage


fromNetpbmImage
  :: forall cs e . ColorModel cs e =>
     Netpbm.PPM -> Maybe (Image S cs e)
fromNetpbmImage Netpbm.PPM {..} = do
  let m = ppmHeight ppmHeader
      n = ppmWidth ppmHeader
  case ppmData of
    PbmPixelData v      -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.Y Bit)
                              fromNetpbmImageUnsafe m n v
    PgmPixelData8 v     -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.Y Word8)
                              fromNetpbmImageUnsafe m n v
    PgmPixelData16 v    -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.Y Word16)
                              fromNetpbmImageUnsafe m n v
    PpmPixelDataRGB8 v  -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word8)
                              fromNetpbmImageUnsafe m n v
    PpmPixelDataRGB16 v -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word16)
                              fromNetpbmImageUnsafe m n v



instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto PBM) (Image r cs e) where
  decodeM f _ = decodeNetpbmImageAuto f
instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Sequence (Auto PBM)) (Array B Ix1 (Image r cs e)) where
  decodeM f _ = decodeNetpbmImageSequenceAuto f

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto PGM) (Image r cs e) where
  decodeM f _ = decodeNetpbmImageAuto f
instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Sequence (Auto PGM)) (Array B Ix1 (Image r cs e)) where
  decodeM f _ = decodeNetpbmImageSequenceAuto f

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto PPM) (Image r cs e) where
  decodeM f _ = decodeNetpbmImageAuto f
instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Sequence (Auto PPM)) (Array B Ix1 (Image r cs e)) where
  decodeM f _ = decodeNetpbmImageSequenceAuto f

fromNetpbmImageAuto
  :: forall cs i e r . (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
     Netpbm.PPM -> Maybe (Image r cs e)
fromNetpbmImageAuto Netpbm.PPM {..} = do
  let m = ppmHeight ppmHeader
      n = ppmWidth ppmHeader
  case ppmData of
    PbmPixelData v -> convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S (Y D65) Bit))
    PgmPixelData8 v ->
      convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S (Y D65) Word8))
    PgmPixelData16 v ->
      convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S (Y D65) Word16))
    PpmPixelDataRGB8 v ->
      convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S SRGB Word8))
    PpmPixelDataRGB16 v ->
      convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S SRGB Word16))
