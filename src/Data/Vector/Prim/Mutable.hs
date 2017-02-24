{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Prim.Mutable
  ( MVector(..)
  -- * Accessors
  -- ** Length information
  , length
  , null
  -- ** Extracting subvectors
  , slice
  -- * Construction
  , new
  , unsafeNew
  , create
  , createM
  , replicate
  , replicateM
  , clone
  -- * Accessing individual elements
  , read
  , write
  , modify
  , swap
  , unsafeRead
  , unsafeWrite
  , unsafeModify
  , unsafeSwap
  -- * Filling and copying
  , set
  , copy
  , move
  , unsafeCopy
  , unsafeMove
  ) where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.Vector.Prim.Internal (loopM_)
import           Prelude                   hiding (length, null, replicate, read)

data MVector s a =
  MVector {-# UNPACK #-}!Int
          {-# UNPACK #-}!(MutableByteArray s)


instance Show (MVector s a) where
  show (MVector k _) = "MVector <" ++ show k ++ ">"

newMVector :: forall a m . (PrimMonad m, Prim a)
    => Int -- ^ Size of the vector
    -> m (MVector (PrimState m) a)
newMVector !k = fmap (MVector k) (newByteArray (k * sizeOf (undefined :: a)))
{-# INLINE newMVector #-}

-- ^ Get size of a single element in number of bytes
eltSize :: forall s a. Prim a => MVector s a -> Int
eltSize _ = sizeOf (undefined :: a)
{-# INLINE eltSize #-}

errorMVector :: String -> String -> a
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
unsafeSlice offset k' mv@(MVector k ma) = do
  mv'@(MVector _ ma') <- unsafeNew k'
  move mv' 0 mv offset k'
  return mv;
  where
    !es = eltSize mv
{-# INLINE unsafeSlice #-}

new :: (PrimMonad m, Prim a)
    => Int -- ^ Size of the vector
    -> m (MVector (PrimState m) a)
new !k | k >= 0 = unsafeNew k
       | otherwise = errorMVector "new" $ "negative size: " ++ show k
{-# INLINE new #-}


unsafeNew :: (PrimMonad m, Prim a)
    => Int -- ^ Size of the vector
    -> m (MVector (PrimState m) a)
unsafeNew = newMVector
{-# INLINE unsafeNew #-}


create :: (Prim a, PrimMonad m)
       => Int -- ^ Size of the vector
       -> (Int -> a)
       -> m (MVector (PrimState m) a)
create !k f = do
  mv <- new k
  loopM_ 0 (+1) (<k) $ \ !i -> unsafeWrite mv i (f i)
  return mv
{-# INLINE create #-}


createM :: (Prim a, PrimMonad m)
        => Int -- ^ Size of the vector
        -> (Int -> m a)
        -> m (MVector (PrimState m) a)
createM !k f = do
  mv <- new k
  loopM_ 0 (+1) (<k) $ \ !i -> f i >>= unsafeWrite mv i
  return mv
{-# INLINE createM #-}


replicate :: (PrimMonad m, Prim a)
          => Int -- ^ Size of the vector
          -> a -- ^ Element to replicate
          -> m (MVector (PrimState m) a)
replicate !k !x = do
  mv <- new (max 0 k)
  set mv x
  return mv
{-# INLINE replicate #-}


replicateM :: (PrimMonad m, Prim a)
          => Int -- ^ Size of the vector
          -> m a -- ^ Element to replicate
          -> m (MVector (PrimState m) a)
replicateM !k f = do
  let !k' = max 0 k
  mv <- new k
  loopM_ 0 (+1) (<k') $ \ !i -> f >>= unsafeWrite mv i
  return mv
{-# INLINE replicateM #-}


clone :: (PrimMonad m, Prim a)
      => (MVector (PrimState m) a)
      -> m (MVector (PrimState m) a)
clone mv@(MVector k _) = do
  mv' <- unsafeNew k
  unsafeCopy mv' mv
  return mv'
{-# INLINE clone #-}


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
write mv@(MVector k _) !i
  | i < 0 || i >= k =
    errorMVector "write" $
    "index: " ++ show i ++ " is out of bounds: " ++ show mv
  | otherwise = unsafeWrite mv i
{-# INLINE write #-}


modify :: (PrimMonad m, Prim a)
       => MVector (PrimState m) a
       -> Int -- ^ Index
       -> (a -> a) -- ^ Modifying function
       -> m ()
modify !mv@(MVector k _) !i
  | i < 0 || i >= k =
    errorMVector "write" $
    "index: " ++ show i ++ " is out of bounds: " ++ show mv
  | otherwise = unsafeModify mv i
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
unsafeModify !mv !i f = unsafeRead mv i >>= (unsafeWrite mv i . f)
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



set :: (PrimMonad m, Prim a)
    => MVector (PrimState m) a
    -> a -- ^ Value to set
    -> m ()
set (MVector k ma) x = setByteArray ma 0 k x
{-# INLINE set #-}


copy :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a -- ^ Target vector
     -> MVector (PrimState m) a -- ^ Source vector
     -> m ()
copy mv'@(MVector k' ma') mv@(MVector k ma)
  | k' == k = unsafeCopy mv' mv
  | otherwise =
    errorMVector "copy" $
    "size mismatch: length " ++ show mv' ++ " /= length " ++ show mv
{-# INLINE copy #-}


unsafeCopy :: (PrimMonad m, Prim a)
           => MVector (PrimState m) a -- ^ Target vector
           -> MVector (PrimState m) a -- ^ Source vector
           -> m ()
unsafeCopy (MVector _ ma') mv@(MVector k ma) =
  copyMutableByteArray ma' 0 ma 0 (k * eltSize mv)
{-# INLINE unsafeCopy #-}


move :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a -- ^ Target vector
     -> Int -- ^ Offset into destination vector
     -> MVector (PrimState m) a -- ^ Source vector
     -> Int -- ^ Offset into source vector
     -> Int -- ^ Number of elements to copy
     -> m ()
move mv'@(MVector k' ma') !offsetD mv@(MVector k ma) !offsetS !n
  | offsetD < 0 || offsetD + n > k' =
    errorMVector "move" $
    "destination size is out of bounds. offset: " ++
    show offsetD ++ ", num: " ++ show n ++ " destination: " ++ show mv'
  | offsetS < 0 || offsetS + n > k =
    errorMVector "move" $
    "source size is out of bounds. offset: " ++
    show offsetS ++ ", num: " ++ show n ++ " source: " ++ show mv
  | otherwise = unsafeMove mv' offsetD mv offsetS n
{-# INLINE move #-}


unsafeMove :: (PrimMonad m, Prim a)
           => MVector (PrimState m) a -- ^ Target vector
           -> Int -- ^ Offset into destination vector
           -> MVector (PrimState m) a -- ^ Source vector
           -> Int -- ^ Offset into source vector
           -> Int -- ^ Number of elements to copy
           -> m ()
unsafeMove (MVector _ ma') !offsetD mv@(MVector k ma) !offsetS !n =
  moveByteArray ma' (offsetD * es) ma (offsetS * es) (n * es)
  where
    !es = eltSize mv
{-# INLINE unsafeMove #-}
