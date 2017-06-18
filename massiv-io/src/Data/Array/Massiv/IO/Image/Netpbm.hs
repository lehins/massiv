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

import           Control.Monad                          (when)
import           Data.Array.Massiv                      as M
import           Data.Array.Massiv.IO.Base
import           Data.Array.Massiv.IO.Image.JuicyPixels (decodeEither, toAnyCS)
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
  decode _ _ bs = decodePPM bs >>= decodeEither showNetpbmCS fromNetpbmImage


instance ColorSpace cs e => Readable (Auto PBM) (Image S cs e) where
  decode _ _ bs = decodePPM bs >>= decodeEither showNetpbmCS fromAnyNetpbmImage


instance ColorSpace cs e => Readable (Sequence PBM) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodeNetpbm fromNetpbmImage

instance ColorSpace cs e => Readable (Sequence (Auto PBM)) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodeNetpbm fromAnyNetpbmImage



-- | Netpbm: portable graymap image with @.pgm@ extension.
data PGM = PGM deriving Show

instance FileFormat PGM where
  ext _ = ".pgm"


instance ColorSpace cs e => Readable PGM (Image S cs e) where
  decode _ _ bs = decodePPM bs >>= decodeEither showNetpbmCS fromNetpbmImage


instance ColorSpace cs e => Readable (Auto PGM) (Image S cs e) where
  decode _ _ bs = decodePPM bs >>= decodeEither showNetpbmCS fromAnyNetpbmImage


instance ColorSpace cs e => Readable (Sequence PGM) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodeNetpbm fromNetpbmImage


instance ColorSpace cs e => Readable (Sequence (Auto PGM)) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodeNetpbm fromAnyNetpbmImage


-- | Netpbm: portable pixmap image with @.ppm@ extension.
data PPM = PPM deriving Show

instance FileFormat PPM where
  ext _ = ".ppm"


instance ColorSpace cs e => Readable PPM (Image S cs e) where
  decode _ _ bs = decodePPM bs >>= decodeEither showNetpbmCS fromNetpbmImage


instance ColorSpace cs e => Readable (Auto PPM) (Image S cs e) where
  decode _ _ bs = decodePPM bs >>= decodeEither showNetpbmCS fromAnyNetpbmImage

instance ColorSpace cs e => Readable (Sequence PPM) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodeNetpbm fromNetpbmImage

instance ColorSpace cs e => Readable (Sequence (Auto PPM)) (Array B DIM1 (Image S cs e)) where
  decode _ _ = decodeNetpbm fromAnyNetpbmImage



--------------------------------------------------------------------------------
-- Decoding images using Netpbm ------------------------------------------------
--------------------------------------------------------------------------------


-- instance Readable (Image S Y Double) PBM where
--   decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

-- instance Readable (Image S Y Double) PGM where
--   decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

-- instance Readable (Image S Y Double) PPM where
--   decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

-- instance Readable (Image S YA Double) PPM where
--   decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

-- instance Readable (Image S RGB Double) PPM where
--   decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm

-- instance Readable (Image S RGBA Double) PPM where
--   decode _ = fmap (ppmToImageUsing pnmDataToImage . head) . decodePnm


-- instance Readable (Image S X Bit) PBM where
--   decode _ = either Left (ppmToImageUsing pnmDataPBMToImage . head) . decodePnm

-- instance Readable (Image S Y Word8) PGM where
--   decode _ = either Left (ppmToImageUsing pnmDataPGM8ToImage . head) . decodePnm

-- instance Readable (Image S Y Word16) PGM where
--   decode _ = either Left (ppmToImageUsing pnmDataPGM16ToImage . head) . decodePnm

-- instance Readable (Image S RGB Word8) PPM where
--   decode _ = either Left (ppmToImageUsing pnmDataPPM8ToImage . head) . decodePnm

-- instance Readable (Image S RGB Word16) PPM where
--   decode _ = either Left (ppmToImageUsing pnmDataPPM16ToImage . head) . decodePnm


-- instance Readable [Image S X Bit] (Seq PBM) where
--   decode _ = pnmToImagesUsing pnmDataPBMToImage

-- instance Readable [Image S Y Word8] (Seq PGM) where
--   decode _ = pnmToImagesUsing pnmDataPGM8ToImage

-- instance Readable [Image S Y Word16] (Seq PGM) where
--   decode _ = pnmToImagesUsing pnmDataPGM16ToImage

-- instance Readable [Image S RGB Word8] (Seq PPM) where
--   decode _ = pnmToImagesUsing pnmDataPPM8ToImage

-- instance Readable [Image S RGB Word16] (Seq PPM) where
--   decode _ = pnmToImagesUsing pnmDataPPM16ToImage


-- decodePnm :: B.ByteString -> Either String [PNM.PPM]
-- decodePnm = pnmResultToImage . PNM.parsePPM where
--   pnmResultToImage (Right ([], _))   = pnmError "Cannot parse PNM image"
--   pnmResultToImage (Right (ppms, _)) = Right ppms
--   pnmResultToImage (Left err)        = pnmError err



decodePPMs :: B.ByteString -> Either String [Netpbm.PPM]
decodePPMs bs = do
  (ppms, _) <- parsePPM bs
  when (P.null ppms) $ Left "Cannot parse PNM image"
  return ppms


-- decodeNetpbm :: ColorSpace cs e =>
--                 B.ByteString -> Either String (Array B DIM1 (Image S cs e))
-- decodeNetpbm bs = do
--   jpImgsLs <- decodePPMs bs
--   case sequence $ fmap fromNetpbmImage jpImgsLs of
--     Nothing     -> Left $ "Could not do an appropriate conversion"
--     Just imgsLs -> Right $ fromListS1D imgsLs
-- {-# INLINE decodeNetpbm #-}


decodeNetpbm :: ColorSpace cs e =>
                   (Netpbm.PPM -> Maybe (Image S cs e))
                   -> B.ByteString -> Either String (Array B DIM1 (Image S cs e))
decodeNetpbm converter bs = do
  jpImgsLs <- decodePPMs bs
  case sequence $ fmap converter jpImgsLs of
    Nothing     -> Left $ "Could not do an appropriate conversion"
    Just imgsLs -> Right $ fromListS1D imgsLs
{-# INLINE decodeNetpbm #-}



decodePPM :: B.ByteString -> Either String Netpbm.PPM
decodePPM bs = do
  (ppms, _) <- parsePPM bs
  case ppms of
    []      -> Left "Cannot parse PNM image"
    (ppm:_) -> Right ppm


-- | TODO: validate sizes
fromNetpbmImageUnsafe
  :: (Storable a, Storable (Pixel cs e))
  => (Int, Int) -> V.Vector a -> Maybe (Image S cs e)
fromNetpbmImageUnsafe sz = fromVector sz . V.unsafeCast


-- pnmToImagesUsing :: (Int -> Int -> PNM.PpmPixelData -> Either String b)
--                  -> B.ByteString -> Either String [b]
-- pnmToImagesUsing conv =
--   fmap (fmap (either error id . ppmToImageUsing conv)) . decodePnm


-- ppmToImageUsing :: (Int -> Int -> PNM.PpmPixelData -> t) -> PNM.PPM -> t
-- ppmToImageUsing conv PNM.PPM {PNM.ppmHeader = PNM.PPMHeader {PNM.ppmWidth = w
--                                                             ,PNM.ppmHeight = h}
--                              ,PNM.ppmData = ppmData} = conv w h ppmData


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


fromAnyNetpbmImage
  :: forall cs e . (ColorSpace cs e, V.Storable (Pixel cs e)) =>
     Netpbm.PPM -> Maybe (Image S cs e)
fromAnyNetpbmImage Netpbm.PPM {..} = do
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


-- pnmDataToImage
--   :: (Convertible cs e, ColorSpace cs e, V.Storable (Pixel cs e)) =>
--      Int -> Int -> PNM.PpmPixelData -> Image S cs e
-- pnmDataToImage w h (PNM.PbmPixelData v)      =
--   convert (makeImageUnsafe (h, w) v :: Image S X Bit)
-- pnmDataToImage w h (PNM.PgmPixelData8 v)     =
--   convert (makeImageUnsafe (h, w) v :: Image S Y Word8)
-- pnmDataToImage w h (PNM.PgmPixelData16 v)    =
--   convert (makeImageUnsafe (h, w) v :: Image S Y Word16)
-- pnmDataToImage w h (PNM.PpmPixelDataRGB8 v)  =
--   convert (makeImageUnsafe (h, w) v :: Image S RGB Word8)
-- pnmDataToImage w h (PNM.PpmPixelDataRGB16 v) =
--   convert (makeImageUnsafe (h, w) v :: Image S RGB Word16)



-- pnmDataPBMToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S X Bit)
-- pnmDataPBMToImage w h (PNM.PbmPixelData v) = Right $ makeImageUnsafe (h, w) v
-- pnmDataPBMToImage _ _ d                    = pnmCSError "Binary (Pixel X Bit)" d

-- pnmDataPGM8ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S Y Word8)
-- pnmDataPGM8ToImage w h (PNM.PgmPixelData8 v) = Right $ makeImageUnsafe (h, w) v
-- pnmDataPGM8ToImage _ _ d                     = pnmCSError "Y8 (Pixel Y Word8)" d

-- pnmDataPGM16ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S Y Word16)
-- pnmDataPGM16ToImage w h (PNM.PgmPixelData16 v) = Right $ makeImageUnsafe (h, w) v
-- pnmDataPGM16ToImage _ _ d                      = pnmCSError "Y16 (Pixel Y Word16)" d

-- pnmDataPPM8ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S RGB Word8)
-- pnmDataPPM8ToImage w h (PNM.PpmPixelDataRGB8 v) = Right $ makeImageUnsafe (h, w) v
-- pnmDataPPM8ToImage _ _ d                        = pnmCSError "RGB8 (Pixel RGB Word8)" d

-- pnmDataPPM16ToImage :: Int -> Int -> PNM.PpmPixelData -> Either String (Image S RGB Word16)
-- pnmDataPPM16ToImage w h (PNM.PpmPixelDataRGB16 v) = Right $ makeImageUnsafe (h, w) v
-- pnmDataPPM16ToImage _ _ d                         = pnmCSError "RGB16 (Pixel RGB Word16)" d


-- ppmToImageUsing :: (Int -> Int -> PNM.PpmPixelData -> t) -> PNM.PPM -> t
-- ppmToImageUsing conv PNM.PPM {PNM.ppmHeader = PNM.PPMHeader {PNM.ppmWidth = w
--                                                             ,PNM.ppmHeight = h}
--                              ,PNM.ppmData = ppmData} = conv w h ppmData





-- pnmCSError :: String -> PNM.PpmPixelData -> Either String a
-- pnmCSError cs ppmData =
--   pnmError $
--   "Input image is in " ++
--   pnmShowData ppmData ++ ", cannot convert it to " ++ cs ++ " colorspace."

-- pnmShowData :: PNM.PpmPixelData -> String
-- pnmShowData (PNM.PbmPixelData _)      = "Binary (Pixel X Bit)"
-- pnmShowData (PNM.PgmPixelData8 _)     = "Y8 (Pixel Y Word8)"
-- pnmShowData (PNM.PgmPixelData16 _)    = "Y16 (Pixel Y Word16)"
-- pnmShowData (PNM.PpmPixelDataRGB8 _)  = "RGB8 (Pixel RGB Word8)"
-- pnmShowData (PNM.PpmPixelDataRGB16 _) = "RGB8 (Pixel RGB Word8)"
