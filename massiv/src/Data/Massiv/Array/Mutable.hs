{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
-- |
-- Module      : Data.Massiv.Array.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable
  ( Mutable
  , MArray
  , msize
  , new
  , thaw
  , freeze
  , read
  , read'
  , write
  , write'
  , modify
  , modify'
  , swap
  , swap'
  -- * Generate (experimental)

  -- $generate
  , generateM
  , generateLinearM
  , mapM
  , imapM
  , forM
  , iforM
  , sequenceM
  ) where

import           Prelude                             hiding (mapM, read)

import           Control.Monad                       (unless)
import           Control.Monad.Primitive             (PrimMonad (..))
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.Core.Common
import           GHC.Base                             (Int (..))
import           GHC.Prim

-- errorSizeMismatch fName sz1 sz2 =
--   error $ fName ++ ": Size mismatch: " ++ show sz1 ++ " /= " ++ show sz2
-- -- TODO: make sure copy is done in parallel as well as sequentially
-- copy mTargetArr sourceArr = do
--   unless (msize mTargetArr == size sourceArr) $
--     errorSizeMismatch "Data.Massiv.Array.Mutable.copy" (msize mTargetArr) (size sourceArr)
--   mSourdceArray <- unsafeThaw sourceArray
--   -- comp from marr
--   -- TODO: use load
--   imapM_ (unsafeWrite)



-- | Initialize a new mutable array. Negative size will result in an empty array.
new :: (Mutable r ix e, PrimMonad m) => ix -> m (MArray (PrimState m) r ix e)
new sz = unsafeNewZero (liftIndex (max 0) sz)
{-# INLINE new #-}

-- | /O(n)/ - Yield a mutable copy of the immutable array
thaw :: (Mutable r ix e, PrimMonad m) => Array r ix e -> m (MArray (PrimState m) r ix e)
thaw = unsafeThaw . clone
{-# INLINE thaw #-}

-- | /O(n)/ - Yield an immutable copy of the mutable array
freeze :: (Mutable r ix e, PrimMonad m) => Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)
freeze comp marr = unsafeFreeze comp marr >>= (return . clone)
{-# INLINE freeze #-}


-- | /O(1)/ - Lookup an element in the mutable array. Return `Nothing` when index is out of bounds.
read :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m (Maybe e)
read marr ix =
  if isSafeIndex (msize marr) ix
  then Just <$> unsafeRead marr ix
  else return Nothing
{-# INLINE read #-}


-- | /O(1)/ - Same as `read`, but throws an error if index is out of bounds.
read' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m e
read' marr ix = do
  mval <- read marr ix
  case mval of
    Just e  -> return e
    Nothing -> errorIx "Data.Massiv.Array.Mutable.read'" (msize marr) ix
{-# INLINE read' #-}


-- | /O(1)/ - Write an element into the cell of a mutable array. Returns `False` when index is out
-- of bounds.
write :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> e -> m Bool
write marr ix e =
  if isSafeIndex (msize marr) ix
  then unsafeWrite marr ix e >> return True
  else return False
{-# INLINE write #-}


-- | /O(1)/ - Same as `write`, but throws an error if index is out of bounds.
write' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> e -> m ()
write' marr ix e =
  write marr ix e >>= (`unless` errorIx "Data.Massiv.Array.Mutable.write'" (msize marr) ix)
{-# INLINE write' #-}


-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied function. Returns
-- `False` when index is out of bounds.
modify :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m Bool
modify marr f ix =
  if isSafeIndex (msize marr) ix
  then do
    val <- unsafeRead marr ix
    unsafeWrite marr ix $ f val
    return True
  else return False
{-# INLINE modify #-}


-- | /O(1)/ - Same as `modify`, but throws an error if index is out of bounds.
modify' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m ()
modify' marr f ix =
  modify marr f ix >>= (`unless` errorIx "Data.Massiv.Array.Mutable.modify'" (msize marr) ix)
{-# INLINE modify' #-}


-- | /O(1)/ - Swap two elements in a mutable array by supplying their indices. Returns `False` when
-- either one of the indices is out of bounds.
swap :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> ix -> m Bool
swap marr ix1 ix2 = do
  let sz = msize marr
  if isSafeIndex sz ix1 && isSafeIndex sz ix2
  then do
    val1 <- unsafeRead marr ix1
    val2 <- unsafeRead marr ix2
    unsafeWrite marr ix1 val2
    unsafeWrite marr ix2 val1
    return True
  else return False
{-# INLINE swap #-}


-- | /O(1)/ - Same as `swap`, but throws an error if index is out of bounds.
swap' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> ix -> m ()
swap' marr ix1 ix2 = do
  success <- swap marr ix1 ix2
  unless success $
    errorIx "Data.Massiv.Array.Mutable.swap'" (msize marr) $
    if isSafeIndex (msize marr) ix1
      then ix2
      else ix1
{-# INLINE swap' #-}


unsafeLinearFillM :: (Mutable r ix e, Monad m) =>
                     MArray RealWorld r ix e -> (Int -> m e) -> WorldState -> m WorldState
unsafeLinearFillM ma f (State s_#) = go 0# s_#
  where
    !(I# k#) = totalElem (msize ma)
    go i# s# =
      case i# <# k# of
        0# -> return (State s#)
        _ -> do
          let i = I# i#
          res <- f i
          State s'# <- unsafeLinearWriteA ma i res (State s#)
          go (i# +# 1#) s'#
{-# INLINE unsafeLinearFillM #-}


-- | /O(n)/ - Same as `generateM` but using a flat index.
--
-- @since 0.1.1
generateLinearM :: (Monad m, Mutable r ix e) => Comp -> ix -> (Int -> m e) -> m (Array r ix e)
generateLinearM comp sz f = do
  (s, mba) <- unsafeNewA (liftIndex (max 0) sz) (State (noDuplicate# realWorld#))
  s' <- unsafeLinearFillM mba f s
  (_, ba) <- unsafeFreezeA comp mba s'
  return ba
{-# INLINE generateLinearM #-}

-- | /O(n)/ - Generate an array monadically using it's mutable interface. Computation will be done
  -- sequentially, regardless of `Comp` argument.
--
-- @since 0.1.1
generateM :: (Monad m, Mutable r ix e) => Comp -> ix -> (ix -> m e) -> m (Array r ix e)
generateM comp sz f = generateLinearM comp sz (f . fromLinearIndex sz)
{-# INLINE generateM #-}


-- | /O(n)/ - Map an index aware monadic action over an Array. This operation will force computation
-- sequentially and will result in a manifest Array.
--
-- @since 0.1.1
imapM
  :: (Monad m, Source r ix e, Mutable r' ix e') =>
     r' -> (ix -> e -> m e') -> Array r ix e -> m (Array r' ix e')
imapM _ f arr =
  generateLinearM (getComp arr) sz (\ !i -> f (fromLinearIndex sz i) (unsafeLinearIndex arr i))
  where
    !sz = size arr
{-# INLINE imapM #-}

-- | /O(n)/ - Map a monadic action over an Array. This operation will force computation sequentially
-- and will result in a manifest Array.
--
-- @since 0.1.1
--
-- ====__Examples__
--
-- >>> mapM P (\i -> Just (i*i)) $ range Seq 0 5
-- Just (Array P Seq (5)
--   [ 0,1,4,9,16 ])
--
mapM
  :: (Monad m, Source r ix e, Mutable r' ix e') =>
     r' -> (e -> m e') -> Array r ix e -> m (Array r' ix e')
mapM r f = imapM r (const f)
{-# INLINE mapM #-}


-- | /O(n)/ - Same as `mapM`, but with its arguments flipped.
--
-- @since 0.1.1
forM ::
     (Monad m, Source r ix e, Mutable r' ix e')
  => r'
  -> Array r ix e
  -> (e -> m e')
  -> m (Array r' ix e')
forM r = flip (mapM r)
{-# INLINE forM #-}


-- | /O(n)/ - Same as `imapM`, but with its arguments flipped.
--
-- @since 0.1.1
iforM :: (Monad m, Source r ix e, Mutable r' ix e') =>
         r' -> Array r ix e -> (ix -> e -> m e') -> m (Array r' ix e')
iforM r = flip (imapM r)
{-# INLINE iforM #-}


-- | /O(n)/ - Sequence monadic actions in a source Array. This operation will force the computation
-- sequentially and will result in a manifest Array.
--
-- @since 0.1.1
sequenceM
  :: (Monad m, Source r ix (m e), Mutable r' ix e) =>
     r' -> Array r ix (m e) -> m (Array r' ix e)
sequenceM r = mapM r id
{-# INLINE sequenceM #-}


{- $generate

Functions in this sections can monadically generate manifest arrays using their associated mutable
interface. Due to the sequential nature of monads generation is done also sequentially regardless of
supplied computation strategy. All of functions here are very much experimental, so please
<https://github.com/lehins/massiv/issues/new report an issue> if you see something not working
properly.

Here is a very imperative like for loop that creates an array while performing a side effect for
each newly created element:

@
printSquare :: Int -> IO (Array P Ix1 Int)
printSquare n = forM P (range Seq 0 n) $ \i -> do
  let e = i*i
  putStrLn $ "Element at index: " ++ show i ++ " = " ++ show e ++ ";"
  return e
@

>>> printSquare 5
Element at index: 0 = 0;
Element at index: 1 = 1;
Element at index: 2 = 4;
Element at index: 3 = 9;
Element at index: 4 = 16;
(Array P Seq (5)
  [ 0,1,4,9,16 ])

-}
