{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv
    -- * Intro
    --
    -- Massiv (Масси́в) is a Russian word for an array, so everywhere you see
    -- this word you can safely assume that it is the intended meaning. It
    -- is pronounced like English word _massive_, except with an accent on
    -- _i_. There is also a data type family `Array` in this library, which is a
    -- more general way to deal with multi-dimensional arrays. The benefit of
    -- using the wrapper `Massiv` data type instead, is that it will
    -- automatically handle fusion and deal with underlying representation for
    -- you, while with `Array` you have to keep track of it manually.
  ( Massiv
  , Comp(..)
  -- * Construction
  , makeMassiv
  , makeVector
  , unwrapMassiv
  -- ** Range
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  -- * Accessors
  -- ** Size information
  --, size
  -- * Mapping
  , map
  , imap
  -- * Monadic Mapping
  , mapM_
  , imapM_
  , forM_
  , iforM_
  -- , mapP_
  -- , imapP_
  -- * Zipping
  , zip
  , zip3
  , unzip
  , unzip3
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  , toListIx1
  , toListIx2
  -- * Folding
  , foldlS
  , foldrS
  , foldlP
  , sum
  , transpose
  -- * Indexing
  , (!)
  -- * Slicing
  , (!>)
  -- * Stencil
  , A.Stencil
  , A.makeStencil
  , A.mkConvolutionStencil
  , mapStencil
  -- * Adding custom types
  , Layout(..)
  , A.Prim
  , A.Unbox
  , A.Storable
  , NFData
  , module Data.Array.Massiv.Common.Ix
  ) where

import           Control.DeepSeq                    (NFData)
import           Data.Array.Massiv.Common           as A
import           Data.Array.Massiv.Common.Ix
import qualified Data.Array.Massiv.Common.Shape     as A
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Delayed.Windowed
import           Data.Array.Massiv.Internal
import qualified Data.Array.Massiv.Manifest         as A
-- import qualified Data.Array.Massiv.Mutable          as A
import qualified Data.Array.Massiv.Ops              as A
import qualified Data.Array.Massiv.Stencil          as A
import           Prelude                            as P hiding (length, map,
                                                          mapM_, null, sum,
                                                          unzip, unzip3, zip,
                                                          zip3, zipWith,
                                                          zipWith3)
import           System.IO.Unsafe                   (unsafePerformIO)

-- | Generate `Massiv` of a specified size using a function that creates its
-- elements. All further computation on generated Massiv will be done according
-- the `Comp` strategy supplied.
makeMassiv :: Layout ix e =>
              Comp -- ^ Computation strategy. Useful constructors are `Seq` and `Par`
           -> ix -- ^ Size of the result `Massiv`
           -> (ix -> e) -- ^ Function to generate elements for a every index in
                        -- the result `Massiv`
           -> Massiv ix e
makeMassiv c sz f = computeM (A.makeArrayR D c sz f)
{-# INLINE [~1] makeMassiv #-}


-- | Same as `makeMassiv`, but type restricted to a flat `Int` index vector.
makeVector :: Layout Ix1 e => Comp -> Ix1 -> (Ix1 -> e) -> Massiv Ix1 e
makeVector = makeMassiv
{-# INLINE [~1] makeVector #-}


range :: Comp -> Int -> Int -> Massiv Ix1 Int
range comp from = computeM . A.range comp from
{-# INLINE [~1] range #-}

rangeStep :: Comp -> Int -> Int -> Int -> Massiv Ix1 Int
rangeStep comp from step = computeM . A.rangeStep comp from step
{-# INLINE [~1] rangeStep #-}

enumFromN :: (Num e, Layout Ix1 e) => Comp -> e -> Ix1 -> Massiv Ix1 e
enumFromN comp from = computeM . A.enumFromN comp from
{-# INLINE [~1] enumFromN #-}

enumFromStepN :: (Num e, Layout Ix1 e) => Comp -> e -> e -> Ix1 -> Massiv Ix1 e
enumFromStepN comp from step = computeM . A.enumFromStepN comp from step
{-# INLINE [~1] enumFromStepN #-}



-- | Map a function over Massiv
map :: (Layout ix e, Layout ix e') => (e' -> e) -> Massiv ix e' -> Massiv ix e
map f = computeM . A.map f . delayM
{-# INLINE [~1] map #-}

-- | Map an index aware function over Massiv
imap :: (Layout ix e, Layout ix e') => (ix -> e' -> e) -> Massiv ix e' -> Massiv ix e
imap f = computeM . A.imap f . delayM
{-# INLINE [~1] imap #-}


-- | Zip two arrays
zip :: (Layout ix (a, b), Layout ix b, Layout ix a) =>
       Massiv ix a -> Massiv ix b -> Massiv ix (a, b)
zip = zipWith (,)
{-# INLINE [~1] zip #-}

-- | Zip three arrays
zip3 :: (Layout ix c, Layout ix b, Layout ix a, Layout ix (a, b, c))
     => Massiv ix a -> Massiv ix b -> Massiv ix c -> Massiv ix (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE [~1] zip3 #-}

-- | Unzip two arrays
unzip :: (Layout ix a, Layout ix b, Layout ix (a, b)) =>
          Massiv ix (a, b) -> (Massiv ix a, Massiv ix b)
unzip m = (map fst m, map snd m)
{-# INLINE [~1] unzip #-}

-- | Unzip three arrays
unzip3 :: (Layout ix a, Layout ix b, Layout ix c, Layout ix (a, b, c)) =>
          Massiv ix (a, b, c) -> (Massiv ix a, Massiv ix b, Massiv ix c)
unzip3 arr = (map (\ (e, _, _) -> e) arr, map (\ (_, e, _) -> e) arr, map (\ (_, _, e) -> e) arr)
{-# INLINE [~1] unzip3 #-}

-- | Zip two arrays with a function. Resulting array will be an intersection of
-- source arrays in case their dimensions do not match.
zipWith :: (Layout ix e1, Layout ix e2, Layout ix e) =>
            (e1 -> e2 -> e)
         -> Massiv ix e1
         -> Massiv ix e2
         -> Massiv ix e
zipWith f a1 a2 = computeM $ A.zipWith f (delayM a1) (delayM a2)
{-# INLINE [~1] zipWith #-}


-- | Just like `zipWith`, except zip three arrays with a function.
zipWith3 :: (Layout ix e1, Layout ix e2, Layout ix e3, Layout ix e) =>
            (e1 -> e2 -> e3 -> e)
         -> Massiv ix e1
         -> Massiv ix e2
         -> Massiv ix e3
         -> Massiv ix e
zipWith3 f = izipWith3 (\ _ e1 e2 e3 -> f e1 e2 e3)
{-# INLINE [~1] zipWith3 #-}


izipWith :: (Layout ix e1, Layout ix e2, Layout ix e) =>
            (ix -> e1 -> e2 -> e)
         -> Massiv ix e1
         -> Massiv ix e2
         -> Massiv ix e
izipWith f a1 a2 = computeM $ A.izipWith f (delayM a1) (delayM a2)
{-# INLINE [~1] izipWith #-}


-- | Just like `zipWith3`, except with an index aware function.
izipWith3 :: (Layout ix e1, Layout ix e2, Layout ix e3, Layout ix e) =>
             (ix -> e1 -> e2 -> e3 -> e)
          -> Massiv ix e1
          -> Massiv ix e2
          -> Massiv ix e3
          -> Massiv ix e
izipWith3 f m1 m2 m3 = computeM $ A.izipWith3 f (delayM m1) (delayM m2) (delayM m3)
{-# INLINE [~1] izipWith3 #-}



-- | Map a monadic function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> mapM_ print $ rangeStep Seq 10 12 60
-- 10
-- 22
-- 34
-- 46
-- 58
--
mapM_ :: (Monad m, Layout ix e) => (e -> m a) -> Massiv ix e -> m ()
mapM_ f = A.mapM_ f . delayM
{-# INLINE [~1] mapM_ #-}


-- | Just like `mapM_`, except with flipped arguments.
--
-- ==== __Examples__
--
-- Here is a common way of iterating N times using a for loop in an imperative
-- language with mutation being an obvious side effect:
--
-- >>> :m + Data.IORef
-- >>> var <- newIORef 0 :: IO (IORef Int)
-- >>> forM_ (range 0 1000) $ \ i -> modifyIORef' var (+i)
-- >>> readIORef var
-- 499500
--
forM_ :: (Monad m, Layout ix e) => Massiv ix e -> (e -> m a) -> m ()
forM_ = flip mapM_
{-# INLINE [~1] forM_ #-}


-- | Map a monadic index aware function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> imapM_ (curry print) $ range 10 15
-- (0,10)
-- (1,11)
-- (2,12)
-- (3,13)
-- (4,14)
--
imapM_ :: (Monad m, Layout ix e) => (ix -> e -> m a) -> Massiv ix e -> m ()
imapM_ f = A.imapM_ f . delayM
{-# INLINE [~1] imapM_ #-}

-- | Just like `imapM_`, except with its arguments flipped.
iforM_ :: (Monad m, Layout ix e) => Massiv ix e -> (ix -> e -> m a) -> m ()
iforM_ = flip imapM_
{-# INLINE [~1] iforM_ #-}



toListIx1 :: Layout ix e => Massiv ix e -> [e]
toListIx1 (Massiv arr) = A.toListIx1 arr
{-# INLINE [~1] toListIx1 #-}



-- | Convert an array with at least 2 dimensions into a list of lists. Inner
-- dimensions will get flattened into a list.
--
-- ==== __Examples__
--
-- >>> toListIx2 $ makeArrayIx2 Seq (2, 3) id
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toListIx2 $ makeArrayIx3 Seq (2, 1, 3) id
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toListIx2 :: (A.Slice (Repr e) ix e, Layout ix e) => Massiv ix e -> [[e]]
toListIx2 (Massiv arr) = A.toListIx2 arr
{-# INLINE [~1] toListIx2 #-}



(!>) :: (Layout (Lower ix) e, Layout ix e) =>
         Massiv ix e -> Int -> Massiv (Lower ix) e
(!>) m i = computeM (delayM m A.!> i)
{-# INLINE [~1] (!>) #-}



(!) :: (Layout ix e) => Massiv ix e -> ix -> e
(!) (Massiv arr) = A.index arr
{-# INLINE [~1] (!) #-}


foldlS :: Layout ix e => (a -> e -> a) -> a -> Massiv ix e -> a
foldlS f acc = A.foldlS f acc . delayM
{-# INLINE [~1] foldlS #-}


foldrS :: Layout ix e => (e -> a -> a) -> a -> Massiv ix e -> a
foldrS f a = A.foldrS f a . delayM
{-# INLINE [~1] foldrS #-}



foldlP :: Layout ix e => (b -> a -> b) -> b -> (a -> e -> a) -> a -> Massiv ix e -> IO b
foldlP g b f a = A.foldlP g b f a . delayM
{-# INLINE [~1] foldlP #-}


sum :: (Num e, Layout ix e) => Massiv ix e -> e
sum = A.sum . unwrapMassiv
{-# INLINE sum #-}



-- | Map a function over Massiv
transpose :: Layout Ix2 e => Massiv Ix2 e -> Massiv Ix2 e
transpose = computeM . A.transpose . delayM
{-# INLINE [~1] transpose #-}

-- Stencil



mapStencil :: (Layout ix e, Layout ix a, Load WD ix a) =>
  A.Stencil ix e a -> Massiv ix e -> Massiv ix a
mapStencil stencil = computeM . A.mapStencil stencil . unwrapMassiv
{-# INLINE mapStencil #-}
