{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Prim.Mutable
  ( MVector(..)
  -- * Accessors
  -- ** Length information
  , length
  , null
  -- ** Extracting subvectors
  , new
  , unsafeNew
  ) where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.Primitive.ByteArray
import           Data.Primitive.Types
import           Data.Vector.Prim.Internal (loopM_)
import           Prelude                   hiding (length, null)


data MVector s a =
  MVector {-# UNPACK #-}!Int
          {-# UNPACK #-}!(MutableByteArray s)


instance Show (MVector s a) where
  show (MVector k _) = "MVector <" ++ show k ++ ">"

newMVector :: forall a m . (PrimMonad m, Prim a)
    => Int -- ^ Size of the vector
    -> m (MVector (PrimState m) a)
newMVector !k = do
  ma <- newByteArray (k * sizeOf (undefined :: a))
  return (MVector k ma)
{-# INLINE newMVector #-}


errorMVector fName errMsg =
  error $ "<Mutable." ++ fName ++ "> " ++ errMsg

length :: MVector s a -> Int
length (MVector k _) = k
{-# INLINE length #-}


null :: MVector s a -> Bool
null (MVector k _) = k == 0
{-# INLINE null #-}


slice :: (PrimMonad m, Prim a)
      => Int -- ^ Start index
      -> Int -- ^ Resulting size
      -> (MVector (PrimState m) a)
      -> m (MVector (PrimState m) a)
slice s e mv@(MVector k _)
  | s + e > k = errorMVector "slice" "end size is higher than original vector"
  | s >= e = errorMVector "slice" "start index is larger than end index"
  | otherwise = unsafeSlice s e mv
{-# INLINE slice #-}


unsafeSlice :: (PrimMonad m, Prim a)
      => Int -- ^ Start index
      -> Int -- ^ Resulting size
      -> (MVector (PrimState m) a)
      -> m (MVector (PrimState m) a)
unsafeSlice s e (MVector k ma) = do
  mv@(MVector _ ma') <- unsafeNew e
  copyMutableByteArray ma' 0 ma s e
  return mv
{-# INLINE unsafeSlice #-}

new :: (PrimMonad m, Prim a)
    => Int -- ^ Size of the vector
    -> m (MVector (PrimState m) a)
new !k | k >= 0 = unsafeNew k
       | otherwise = errorMVector "new" $ "non-positive size: " ++ show k
{-# INLINE new #-}


unsafeNew :: (PrimMonad m, Prim a)
    => Int -- ^ Size of the vector
    -> m (MVector (PrimState m) a)
unsafeNew = newMVector
{-# INLINE unsafeNew #-}


read :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a
     -> Int -- ^ Index
     -> m a
read mv@(MVector k _) !i
  | i < 0 || i >= k =
    errorMVector "read" $
    "index: " ++ show i ++ " is out of bounds: " ++ show mv
  | otherwise = unsafeRead mv i
{-# INLINE read #-}



write :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a
     -> Int -- ^ Index
     -> a -- ^ Element to write
     -> m ()
write mv@(MVector k _) !i !x
  | i < 0 || i >= k =
    errorMVector "write" $
    "index: " ++ show i ++ " is out of bounds: " ++ show mv
  | otherwise = unsafeWrite mv i x
{-# INLINE write #-}


modify :: (PrimMonad m, Prim a)
       => MVector (PrimState m) a
       -> Int -- ^ Index
       -> (a -> a) -- ^ Modifying function
       -> m ()
modify !mv@(MVector k _) !i f
  | i < 0 || i >= k =
    errorMVector "write" $
    "index: " ++ show i ++ " is out of bounds: " ++ show mv
  | otherwise = unsafeModify mv i f
{-# INLINE modify #-}


swap :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a
     -> Int -- ^ Index of first element
     -> Int -- ^ Index of second element
     -> m ()
swap !mv@(MVector k _) !i1 !i2
  | i1 < 0 || i1 >= k =
    errorMVector "write" $
    "first index: " ++ show i1 ++ " is out of bounds: " ++ show mv
  | i2 < 0 || i2 >= k =
    errorMVector "write" $
    "second index: " ++ show i2 ++ " is out of bounds: " ++ show mv
  | otherwise = unsafeSwap mv i1 i2
{-# INLINE swap #-}


unsafeRead :: (PrimMonad m, Prim a)
           => MVector (PrimState m) a
           -> Int -- ^ Index
           -> m a
unsafeRead (MVector _ ma) = readByteArray ma
{-# INLINE unsafeRead #-}


unsafeWrite :: (PrimMonad m, Prim a)
            => MVector (PrimState m) a
            -> Int -- ^ Index
            -> a -- ^ Element to write
            -> m ()
unsafeWrite (MVector _ ma) = writeByteArray ma
{-# INLINE unsafeWrite #-}


unsafeModify :: (PrimMonad m, Prim a)
             => MVector (PrimState m) a
             -> Int -- ^ Index
             -> (a -> a) -- ^ Modifying function
             -> m ()
unsafeModify !mv !i f = do
  x <- unsafeRead mv i
  unsafeWrite mv i (f x)
{-# INLINE unsafeModify #-}



unsafeSwap :: (PrimMonad m, Prim a)
             => MVector (PrimState m) a
             -> Int -- ^ Index of first element
             -> Int -- ^ Index of second element
             -> m ()
unsafeSwap !mv !i1 !i2 = do
  x1 <- unsafeRead mv i1
  x2 <- unsafeRead mv i2
  unsafeWrite mv i1 x2
  unsafeWrite mv i2 x1
{-# INLINE unsafeSwap #-}



--replicate
