{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Prim.Internal where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.Primitive.ByteArray
import           Data.Primitive.Types

data Vector a =
  Vector {-# UNPACK #-}!Int
         {-# UNPACK #-}!ByteArray


loopM_ :: Monad m => t -> (t -> t) -> (t -> Bool) -> (t -> m a) -> m ()
loopM_ !init next cond f = go init where
  go !step =
    case cond step of
      False -> return ()
      True  -> f step >> go (next step)
  {-# INLINE go #-}
{-# INLINE loopM_ #-}



generateM :: forall a m . (Prim a, PrimMonad m)
         => Int -- ^ Size of the vector
         -> (Int -> a) -> m (Vector a)
generateM !k f = do
  mv <- newByteArray (k * sizeOf (undefined :: a))
  loopM_ 0 (+1) (<k) $ \ !i -> do
    writeByteArray mv i (f i)
  v <- unsafeFreezeByteArray mv
  return $ Vector k v
{-# INLINE generateM #-}
