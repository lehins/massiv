{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.Image.IO.Formats.Netpbm
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Formats.Netpbm
  ( -- * Netpbm formats
    -- ** PBM
    PBM(..)
    -- ** PGM
  , PGM(..)
    -- ** PPM
  , PPM(..)
  ) where

import qualified Data.ByteString                 as B (ByteString)
import qualified Data.Vector.Storable            as V
import           Foreign.Storable                (Storable)
import           Graphics.Image.ColorSpace
import           Graphics.Image.Internal         as I
import           Graphics.Image.IO.Base
import qualified Graphics.Netpbm                 as PNM


-- | Netpbm: portable bitmap image with @.pbm@ extension.
data PBM = PBM deriving Show

instance ImageFormat PBM where
  data SaveOption PBM

  ext _ = ".pbm"


-- | Netpbm: portable graymap image with @.pgm@ extension.
data PGM = PGM deriving Show

instance ImageFormat PGM where
  data SaveOption PGM

  ext _ = ".pgm"


-- | Netpbm: portable pixmap image with @.ppm@ extension.
data PPM = PPM deriving Show

instance ImageFormat PPM where
  data SaveOption PPM

  ext _ = ".ppm"


instance ImageFormat (Seq PBM) where
  data SaveOption (Seq PBM)

  ext _ = ".pbm"


instance ImageFormat (Seq PGM) where
  data SaveOption (Seq PGM)

  ext _ = ".pgm"


instance ImageFormat (Seq PPM) where
  data SaveOption (Seq PPM)

  ext _ = ".ppm"


--------------------------------------------------------------------------------
-- Decoding images using Netpbm ------------------------------------------------
--------------------------------------------------------------------------------


instance Readable (Image S Y Double) PBM where
  decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

instance Readable (Image S Y Double) PGM where
  decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

instance Readable (Image S Y Double) PPM where
  decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

instance Readable (Image S YA Double) PPM where
  decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

instance Readable (Image S RGB Double) PPM where
  decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

instance Readable (Image S RGBA Double) PPM where
  decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm


instance Readable (Image S X Bit) PBM where
  decode _ = either Left (ppmToImageUsing pnmDataPBMToImage . head) . decodePnm

instance Readable (Image S Y Word8) PGM where
  decode _ = either Left (ppmToImageUsing pnmDataPGM8ToImage . head) . decodePnm

instance Readable (Image S Y Word16) PGM where
  decode _ = either Left (ppmToImageUsing pnmDataPGM16ToImage . head) . decodePnm

instance Readable (Image S RGB Word8) PPM where
  decode _ = either Left (ppmToImageUsing pnmDataPPM8ToImage . head) . decodePnm

instance Readable (Image S RGB Word16) PPM where
  decode _ = either Left (ppmToImageUsing pnmDataPPM16ToImage . head) . decodePnm


instance Readable [Image S X Bit] (Seq PBM) where
  decode _ = pnmToImagesUsing pnmDataPBMToImage

instance Readable [Image S Y Word8] (Seq PGM) where
  decode _ = pnmToImagesUsing pnmDataPGM8ToImage

instance Readable [Image S Y Word16] (Seq PGM) where
  decode _ = pnmToImagesUsing pnmDataPGM16ToImage

instance Readable [Image S RGB Word8] (Seq PPM) where
  decode _ = pnmToImagesUsing pnmDataPPM8ToImage

instance Readable [Image S RGB Word16] (Seq PPM) where
  decode _ = pnmToImagesUsing pnmDataPPM16ToImage


pnmToImagesUsing :: (Int -> Int -> PNM.PpmPixelData -> Either String b)
                 -> B.ByteString -> Either String [b]
pnmToImagesUsing conv =
  fmap (fmap (either error id . ppmToImageUsing conv)) . decodePnm


pnmDataToImage
  :: (Convertible cs e, ColorSpace cs e, V.Storable (Pixel cs e)) =>
     Int -> Int -> PNM.PpmPixelData -> Image S cs e
pnmDataToImage w h (PNM.PbmPixelData v)      =
  convert (makeImageUnsafe (h, w) v :: Image S X Bit)
pnmDataToImage w h (PNM.PgmPixelData8 v)     =
  convert (makeImageUnsafe (h, w) v :: Image S Y Word8)
pnmDataToImage w h (PNM.PgmPixelData16 v)    =
  convert (makeImageUnsafe (h, w) v :: Image S Y Word16)
pnmDataToImage w h (PNM.PpmPixelDataRGB8 v)  =
  convert (makeImageUnsafe (h, w) v :: Image S RGB Word8)
pnmDataToImage w h (PNM.PpmPixelDataRGB16 v) =
  convert (makeImageUnsafe (h, w) v :: Image S RGB Word16)

-- | TODO: validate sizes
makeImageUnsafe
  :: (Storable a, Storable (Pixel cs e), Array2D S cs e)
  => (Int, Int) -> V.Vector a -> Image S cs e
makeImageUnsafe sz = fromVector sz . V.unsafeCast


pnmDataPBMToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S X Bit)
pnmDataPBMToImage w h (PNM.PbmPixelData v) = Right $ makeImageUnsafe (h, w) v
pnmDataPBMToImage _ _ d                    = pnmCSError "Binary (Pixel X Bit)" d

pnmDataPGM8ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S Y Word8)
pnmDataPGM8ToImage w h (PNM.PgmPixelData8 v) = Right $ makeImageUnsafe (h, w) v
pnmDataPGM8ToImage _ _ d                     = pnmCSError "Y8 (Pixel Y Word8)" d

pnmDataPGM16ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S Y Word16)
pnmDataPGM16ToImage w h (PNM.PgmPixelData16 v) = Right $ makeImageUnsafe (h, w) v
pnmDataPGM16ToImage _ _ d                      = pnmCSError "Y16 (Pixel Y Word16)" d

pnmDataPPM8ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S RGB Word8)
pnmDataPPM8ToImage w h (PNM.PpmPixelDataRGB8 v) = Right $ makeImageUnsafe (h, w) v
pnmDataPPM8ToImage _ _ d                        = pnmCSError "RGB8 (Pixel RGB Word8)" d

pnmDataPPM16ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S RGB Word16)
pnmDataPPM16ToImage w h (PNM.PpmPixelDataRGB16 v) = Right $ makeImageUnsafe (h, w) v
pnmDataPPM16ToImage _ _ d                         = pnmCSError "RGB16 (Pixel RGB Word16)" d


ppmToImageUsing :: (Int -> Int -> PNM.PpmPixelData -> t) -> PNM.PPM -> t
ppmToImageUsing conv PNM.PPM {PNM.ppmHeader = PNM.PPMHeader {PNM.ppmWidth = w
                                                            ,PNM.ppmHeight = h}
                             ,PNM.ppmData = ppmData} = conv w h ppmData


decodePnm :: B.ByteString -> Either String [PNM.PPM]
decodePnm = pnmResultToImage . PNM.parsePPM where
  pnmResultToImage (Right ([], _))   = pnmError "Unknown"
  pnmResultToImage (Right (ppms, _)) = Right ppms
  pnmResultToImage (Left err)        = pnmError err


pnmError :: String -> Either String a
pnmError err = Left ("Netpbm decoding error: "++err)


pnmCSError :: String -> PNM.PpmPixelData -> Either String a
pnmCSError cs ppmData =
  pnmError $
  "Input image is in " ++
  pnmShowData ppmData ++ ", cannot convert it to " ++ cs ++ " colorspace."

pnmShowData :: PNM.PpmPixelData -> String
pnmShowData (PNM.PbmPixelData _)      = "Binary (Pixel X Bit)"
pnmShowData (PNM.PgmPixelData8 _)     = "Y8 (Pixel Y Word8)"
pnmShowData (PNM.PgmPixelData16 _)    = "Y16 (Pixel Y Word16)"
pnmShowData (PNM.PpmPixelDataRGB8 _)  = "RGB8 (Pixel RGB Word8)"
pnmShowData (PNM.PpmPixelDataRGB16 _) = "RGB8 (Pixel RGB Word8)"
