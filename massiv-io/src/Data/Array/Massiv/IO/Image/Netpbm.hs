{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Graphics.Image.IO.Formats.Netpbm
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.IO.Image.Netpbm
  ( -- * Netpbm formats
    -- ** PBM
    PBM(..)
    -- ** PGM
  , PGM(..)
    -- ** PPM
  , PPM(..)
  ) where

import           Control.Exception
import           Data.Array.Massiv                      as M
import           Data.Array.Massiv.IO.Base
import           Data.Array.Massiv.IO.Image.JuicyPixels (toAnyCS)
import qualified Data.ByteString                        as B (ByteString)
import           Data.Typeable
import qualified Data.Vector.Storable                   as V
import           Foreign.Storable                       (Storable)
import           Graphics.ColorSpace
import           Graphics.Netpbm                        as Netpbm hiding (PPM)
import qualified Graphics.Netpbm                        as Netpbm (PPM (..))
import           Prelude                                as P

-- | Netpbm: portable bitmap image with @.pbm@ extension.
data PBM = PBM deriving Show

instance FileFormat PBM where
  ext _ = ".pbm"


instance ColorSpace cs e => Readable PBM (Image S cs e) where
  decode _ _ = decodePPM fromNetpbmImage


instance ColorSpace cs e => Readable (Auto PBM) (Image S cs e) where
  decode _ _ = decodePPM fromNetpbmImageAuto


instance ColorSpace cs e => Readable (Sequence PBM) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodePPMs fromNetpbmImage

instance ColorSpace cs e => Readable (Sequence (Auto PBM)) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodePPMs fromNetpbmImageAuto



-- | Netpbm: portable graymap image with @.pgm@ extension.
data PGM = PGM deriving Show

instance FileFormat PGM where
  ext _ = ".pgm"


instance ColorSpace cs e => Readable PGM (Image S cs e) where
  decode _ _ = decodePPM fromNetpbmImage


instance ColorSpace cs e => Readable (Auto PGM) (Image S cs e) where
  decode _ _ = decodePPM fromNetpbmImageAuto


instance ColorSpace cs e => Readable (Sequence PGM) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodePPMs fromNetpbmImage


instance ColorSpace cs e => Readable (Sequence (Auto PGM)) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodePPMs fromNetpbmImageAuto


-- | Netpbm: portable pixmap image with @.ppm@ extension.
data PPM = PPM deriving Show

instance FileFormat PPM where
  ext _ = ".ppm"


instance ColorSpace cs e => Readable PPM (Image S cs e) where
  decode _ _ = decodePPM fromNetpbmImage

instance ColorSpace cs e => Readable (Auto PPM) (Image S cs e) where
  decode _ _ = decodePPM fromNetpbmImageAuto

instance ColorSpace cs e => Readable (Sequence PPM) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodePPMs fromNetpbmImage

instance ColorSpace cs e => Readable (Sequence (Auto PPM)) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodePPMs fromNetpbmImageAuto



decodePPMs :: ColorSpace cs e =>
                (Netpbm.PPM -> Maybe (Image S cs e))
             -> B.ByteString
             -> Array B DIM1 (Image S cs e)
decodePPMs converter bs =
  either (throw . DecodeError) fromListS1D $
  (P.map (fromEitherDecode showNetpbmCS converter . Right) . fst) <$>
  parsePPM bs
{-# INLINE decodePPMs #-}


decodePPM :: ColorSpace cs e =>
             (Netpbm.PPM -> Maybe (Image S cs e))
          -> B.ByteString
          -> Image S cs e
decodePPM decoder bs = fromEitherDecode showNetpbmCS decoder $ do
  (ppms, _) <- parsePPM bs
  case ppms of
    []      -> Left "Cannot parse PNM image"
    (ppm:_) -> Right ppm
{-# INLINE decodePPM #-}


-- | TODO: validate sizes
fromNetpbmImageUnsafe
  :: (Storable a, Storable (Pixel cs e))
  => (Int, Int) -> V.Vector a -> Maybe (Image S cs e)
fromNetpbmImageUnsafe sz = fromVector sz . V.unsafeCast



showNetpbmCS :: Netpbm.PPM -> String
showNetpbmCS Netpbm.PPM {ppmData} = do
  case ppmData of
    PbmPixelData _      -> "Image S X Bit"
    PgmPixelData8 _     -> "Image S Y Word8"
    PgmPixelData16 _    -> "Image S Y Word16"
    PpmPixelDataRGB8 _  -> "Image S RGB Word8"
    PpmPixelDataRGB16 _ -> "Image S RGB Word16"


fromNetpbmImage
  :: forall cs e . (ColorSpace cs e, V.Storable (Pixel cs e)) =>
     Netpbm.PPM -> Maybe (Image S cs e)
fromNetpbmImage Netpbm.PPM {..} = do
  let sz = (ppmHeight ppmHeader, ppmWidth ppmHeader)
  case ppmData of
    PbmPixelData v      -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel X Bit)
                              fromNetpbmImageUnsafe sz v
    PgmPixelData8 v     -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel Y Word8)
                              fromNetpbmImageUnsafe sz v
    PgmPixelData16 v    -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel Y Word16)
                              fromNetpbmImageUnsafe sz v
    PpmPixelDataRGB8 v  -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel RGB Word8)
                              fromNetpbmImageUnsafe sz v
    PpmPixelDataRGB16 v -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel RGB Word16)
                              fromNetpbmImageUnsafe sz v


fromNetpbmImageAuto
  :: forall cs e . (ColorSpace cs e, V.Storable (Pixel cs e)) =>
     Netpbm.PPM -> Maybe (Image S cs e)
fromNetpbmImageAuto Netpbm.PPM {..} = do
  let sz = (ppmHeight ppmHeader, ppmWidth ppmHeader)
  case ppmData of
    PbmPixelData v ->
      (fromNetpbmImageUnsafe sz v :: Maybe (Image S X Bit)) >>= (toAnyCS . M.map fromPixelBinary)
    PgmPixelData8 v ->
      (fromNetpbmImageUnsafe sz v :: Maybe (Image S Y Word8)) >>= toAnyCS
    PgmPixelData16 v ->
      (fromNetpbmImageUnsafe sz v :: Maybe (Image S Y Word16)) >>= toAnyCS
    PpmPixelDataRGB8 v ->
      (fromNetpbmImageUnsafe sz v :: Maybe (Image S RGB Word8)) >>= toAnyCS
    PpmPixelDataRGB16 v ->
      (fromNetpbmImageUnsafe sz v :: Maybe (Image S RGB Word16)) >>= toAnyCS

