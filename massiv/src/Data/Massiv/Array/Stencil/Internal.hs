{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Internal where

import           Control.Applicative
import           Control.DeepSeq
-- import           Control.Monad               (unless, void, when)
-- import           Control.Monad.Primitive     (PrimMonad)
-- import           Control.Monad.ST
import           Data.Massiv.Core.Common
import           Data.Massiv.Array.Delayed.Internal
-- import           Data.Massiv.Array.Manifest
import           Data.Default.Class                (Default (def))
-- import           Data.Maybe                  (fromJust)
-- import           GHC.Exts                    (inline)

-- import qualified Data.Vector.Unboxed         as VU
-- import qualified Data.Vector.Unboxed.Mutable as MVU


data Stencil ix e a = Stencil
  { stencilBorder :: Border e
  , stencilSize   :: !ix
  , stencilCenter :: !ix
  , stencilFunc   :: (ix -> Value e) -> ix -> Value a
  }

instance (NFData e, Index ix) => NFData (Stencil ix e a) where
  rnf (Stencil b sz ix f) = b `deepseq` sz `deepseq` ix `deepseq` f `seq` ()


newtype Value e = Value { unValue :: e } deriving (Show, Eq, Ord, Bounded)

instance NFData e => NFData (Value e) where
  rnf (Value e) = rnf e

instance Functor Value where
  fmap f (Value e) = Value (f e)
  {-# INLINE fmap #-}

instance Applicative Value where
  pure = Value
  {-# INLINE pure #-}
  (<*>) (Value f) (Value e) = Value (f e)
  {-# INLINE (<*>) #-}

instance Num e => Num (Value e) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = Value . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional e => Fractional (Value e) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating e => Floating (Value e) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  log = fmap log
  {-# INLINE log #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  cos = fmap cos
  {-# INLINE cos #-}
  tan = fmap tan
  {-# INLINE tan #-}
  asin = fmap asin
  {-# INLINE asin #-}
  acos = fmap acos
  {-# INLINE acos #-}
  atan = fmap atan
  {-# INLINE atan #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}


-- data Dependency ix = Dependency { depDimension :: Int
--                                 , depDirection :: Int }

-- data StencilM ix e a = StencilM
--   { mStencilBorder :: Border e
--   , mStencilSize   :: !ix
--   , mStencilCenter :: !ix
--   , mStencilDeps   :: Array M Int Int
--   , mStencilFunc   :: forall m . PrimMonad m => (ix -> e) -> (ix -> m a) -> ix -> m a
--   }


-- -- | __Warning__: Highly experimental and untested, use at your own risk.
-- makeStencilM
--   :: forall ix e a . (Index ix, Default e, Default a)
--   => Border e
--   -> ix
--   -> ix
--   -> (forall m . PrimMonad m => ((ix -> e) -> (ix -> m a) -> m a))
--   -> StencilM ix e a
-- makeStencilM b !sSz !sCenter relStencil =
--   StencilM b sSz sCenter deps stencil
--   where
--     stencil
--       :: (Default a, PrimMonad m)
--       => (ix -> e) -> (ix -> m a) -> ix -> m a
--     stencil getVal getCurValM !ix =
--       inline
--         relStencil
--         (\ !ixD -> getVal (liftIndex2 (-) ix ixD))
--         (\ !ixD -> getCurValM (liftIndex2 (-) ix ixD))
--     {-# INLINE stencil #-}
--     Dim sRank = rank sSz
--     defArrA :: Array D ix a
--     defArrA = DArray Seq sSz (const def)
--     defArr = DArray Seq sSz (const def)
--     deps = toManifest $ fromJust $ castFromVector Seq sRank $ VU.create makeDeps
--     -- TODO: switch to mutable Array, once it is implemented.
--     makeDeps :: ST s (MVU.MVector s Int)
--     makeDeps = do
--       mv <- MVU.new sRank
--       -- Need to record at which dimensions and directions there are dependencies
--       let checkWrite ix = do
--             unless (isSafeIndex sSz ix) $
--               error "mkStaticStencilM: Invalid StencilM index access"
--             loopM_ 0 (< sRank) (+ 1) $ \i -> do
--               let !r =
--                     maybe (errorImpossible "mkStaticStencilM") signum $
--                     getIndex ix (Dim i)
--               when (r /= 0) $ do
--                 curVal <- MVU.read mv i
--                 if curVal == 0
--                   then MVU.write mv i r
--                   else when (curVal /= r) $
--                        error
--                          "mkStaticStencilM: Stencil creates an invalid dependency"
--             return $ unsafeIndex defArrA ix
--           checkRead = return . safeStencilIndex defArrA
--       void $ stencil (safeStencilIndex defArr) checkRead sCenter
--       void $ relStencil (unsafeIndex defArr) checkWrite
--       return mv
-- {-# INLINE makeStencilM #-}



instance Functor (Stencil ix e) where
  fmap f stencil@(Stencil {stencilFunc = g}) = stencil {stencilFunc = stF}
    where
      stF s = Value . f . unValue . g s
      {-# INLINE stF #-}
  {-# INLINE fmap #-}


-- TODO: Figure out interchange law (u <*> pure y = pure ($ y) <*> u) and issue
-- with discarding size and center. Best idea so far is to increase stencil size to
-- the maximum one and shift the center of the other stencil so that they both match
-- up. This approach would also remove requirement to validate the result
-- Stencil - both stencils are trusted, increasing the size will not affect the
-- safety.
instance (Default e, Index ix) => Applicative (Stencil ix e) where
  pure a = Stencil Edge oneIndex zeroIndex (const (const (Value a)))
  {-# INLINE pure #-}
  (<*>) (Stencil _ sSz1 sC1 f1) (Stencil sB sSz2 sC2 f2) =
    validateStencil def (Stencil sB newSz maxCenter stF)
    where
      stF gV !ix = Value ((unValue (f1 gV ix)) (unValue (f2 gV ix)))
      {-# INLINE stF #-}
      !newSz =
        liftIndex2
          (+)
          maxCenter
          (liftIndex2 max (liftIndex2 (-) sSz1 sC1) (liftIndex2 (-) sSz2 sC2))
      !maxCenter = liftIndex2 max sC1 sC2
  {-# INLINE (<*>) #-}

instance (Index ix, Default e, Num a) => Num (Stencil ix e a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Index ix, Default e, Fractional a) => Fractional (Stencil ix e a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Index ix, Default e, Floating a) => Floating (Stencil ix e a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  log = fmap log
  {-# INLINE log #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  cos = fmap cos
  {-# INLINE cos #-}
  tan = fmap tan
  {-# INLINE tan #-}
  asin = fmap asin
  {-# INLINE asin #-}
  acos = fmap acos
  {-# INLINE acos #-}
  atan = fmap atan
  {-# INLINE atan #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}


safeStencilIndex :: Index ix => Array D ix e -> ix -> e
safeStencilIndex DArray {..} ix
  | isSafeIndex dSize ix = dUnsafeIndex ix
  | otherwise =
    error $
    "Index is out of bounds: " ++ show ix ++ " for stencil size: " ++ show dSize


validateStencil
  :: Index ix
  => e -> Stencil ix e a -> Stencil ix e a
validateStencil d s@(Stencil _ sSz sCenter stencil) =
  let valArr = DArray Seq sSz (const d)
  in stencil (Value . safeStencilIndex valArr) sCenter `seq` s
{-# INLINE validateStencil #-}


errorImpossible :: String -> a
errorImpossible fName =
  error $ fName ++ ": Impossible happened. Please report this error."
{-# NOINLINE errorImpossible #-}
