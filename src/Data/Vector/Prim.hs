{-# LANGUAGE BangPatterns        #-}
module Data.Vector.Prim where


import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Primitive
import           Data.Primitive.ByteArray
import           Data.Primitive.Types
import           Data.Vector.Prim.Mutable

data Vector a =
  Vector {-# UNPACK #-}!Int
         {-# UNPACK #-}!ByteArray


freeze
  :: (PrimMonad m, Prim a)
  => (MVector (PrimState m) a)
  -> m (Vector a)
freeze mv = clone mv >>= unsafeFreeze
{-# INLINE freeze #-}


unsafeFreeze
  :: (PrimMonad m, Prim a)
  => (MVector (PrimState m) a)
  -> m (Vector a)
unsafeFreeze (MVector k ma) = fmap (Vector k) (unsafeFreezeByteArray ma)
{-# INLINE unsafeFreeze #-}


generate :: Prim a => Int -> (Int -> a) -> Vector a
generate !k f = runST $ create k f >>= unsafeFreeze
{-# INLINE generate #-}

-- generateM :: forall a m . (Prim a, PrimMonad m)
--          => Int -- ^ Size of the vector
--          -> (Int -> a) -> m (Vector a)
-- generateM !k f = do
--   mv <- newByteArray (k * sizeOf (undefined :: a))
--   loopM_ 0 (+1) (<k) $ \ !i -> do
--     writeByteArray mv i (f i)
--   v <- unsafeFreezeByteArray mv
--   return $ Vector k v
-- {-# INLINE generateM #-}
