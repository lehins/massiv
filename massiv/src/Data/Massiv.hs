{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
-- |
-- Module      : Data.Massiv
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- Massiv (Масси́в) is a Russian word for an array. It is pronounced like English
-- word /massive/, except with an accent on the /i/. There is also a data family
-- `Array` in this library, which gives you a lot more control on ways to
-- compute multi-dimensional arrays, at the expense of a higher learning
-- curve. The major benefits of using the `Massiv` data type instead `Array`, is
-- that it will automatically handle fusion, deal with underlying representation
-- for you and it is will also do computation by default in parlallel.
--
module Data.Massiv
  ( Massiv
  -- * Construction
  , makeMassiv
  , makeVector
  -- * Accessors
  -- ** Size information
  , size
  , reshape
  , length
  , null
  -- ** Indexing
  , (!)
  , (!?)
  , (?)
  -- ** Slicing
  --
  -- All slicing is done in constant time, ignoring computation of the index
  -- offset to read elements from the memory during lookups. Left slicing
  -- essentially follows memory representation, i.e. row-major, therefore it is
  -- the most efficient type of slicing.
  --
  -- Every slicing operation lowers the dimension of an array exactly by one:
  --
  -- prop> rank arr == 1 + rank (arr !> i)
  --
  -- So, in order to take smaller slices, for example taking a row from a
  -- 3-dimensional array, slicing operations can be chained:
  --
  -- >>> let mass = makeMassiv (5 :> 4 :. 3) (\(i :> j :. k) -> (i, j, k))
  -- >>> toListIx1 <$> mass !?> 1 ?> 2
  -- Just [(1,2,0),(1,2,1),(1,2,2)]
  --
  -- This approach can even be used as an alternative to indexing:
  --
  -- >>> mass <!? 2 <? 3 ? 4
  -- Just (4,3,2)
  -- >>> mass !> 4 !> 3 ! 2
  -- (4,3,2)
  -- >>> mass ! 4 :> 3 :. 2
  -- (4,3,2)
  --
  -- *** From the outside
  , (!>)
  , (!?>)
  , (?>)
  -- *** From the inside
  , (<!)
  , (<!?)
  , (<?)
  -- *** From the middle
  , (<!>)
  , (<!?>)
  , (<?>)
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
  -- * Folding
  , foldlS
  , foldrS
  , foldlP
  , sum
  , product
  , transpose
  -- * Range
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  -- * Conversion
  , unwrapMassiv
  , toListIx1
  , toListIx2
  -- * Stencil
  , A.makeStencil
  , A.mkConvolutionStencil
  , mapStencil
  -- * Adding custom types
  , module C
  , module Data.Massiv.Types
  ) where

import           Data.Massiv.Array.Delayed
import qualified Data.Massiv.Array.Manifest as A
import qualified Data.Massiv.Core as C
import           Data.Massiv.Types hiding (Massiv, size, Slice(..))
import           Data.Massiv.Internal
-- import qualified Data.Massiv.Array.Mutable          as A
import qualified Data.Massiv.Array.Ops      as A
import qualified Data.Massiv.Array.Stencil  as A
import           Prelude                    as P hiding (length, map, mapM_,
                                                  null, sum, product, unzip, unzip3, zip,
                                                  zip3, zipWith, zipWith3)
import GHC.Exts

-- | Generate `Massiv` of a specified size using a function that creates its
-- elements. All further computation on generated Massiv will be done according
-- the `Comp` strategy supplied.
makeMassiv :: Layout ix e =>
              ix -- ^ Size of the result `Massiv`
           -> (ix -> e) -- ^ Function to generate elements for a every index in
                        -- the result `Massiv`
           -> Massiv ix e
makeMassiv sz f = computeM (A.makeArrayR D Par sz f)
{-# INLINE [~1] makeMassiv #-}


-- | Same as `makeMassiv`, but type restricted to a flat `Int` index vector.
makeVector :: Layout Ix1 e => Ix1 -> (Ix1 -> e) -> Massiv Ix1 e
makeVector = makeMassiv
{-# INLINE [~1] makeVector #-}


-- | /O(1)/ - Get the size of the Massiv
size :: Layout ix e => Massiv ix e -> ix
size (Massiv arr) = C.size arr
{-# INLINE size #-}

-- | /O(1)/ - Get the length of the Massiv, i.e. total number of elements.
length :: Layout ix e => Massiv ix e -> Int
length = totalElem . size
{-# INLINE length #-}

-- | /O(1)/ - Test wether Massiv is empty.
null :: Layout ix e => Massiv ix e -> Bool
null = (0 ==) . length
{-# INLINE null #-}

reshape :: (Layout ix' e, Layout ix e) => ix' -> Massiv ix e -> Maybe (Massiv ix' e)
reshape sz mass = computeM <$> A.reshape sz (delayM mass)
{-# INLINE reshape #-}


infixl 4 !, !?, ?, !>, !?>, ?>, <!, <!?, <?, <!>, <!?>, <?>

-- | /O(1)/ - Index the Massiv. Throws an error when index is out of bounds.
(!) :: Layout ix e => Massiv ix e -> ix -> e
(!) (Massiv arr) = (arr A.!)
{-# INLINE (!) #-}

-- | /O(1)/ - Index the Massiv. Returns `Nothing` when index is out of bounds.
(!?) :: Layout ix e => Massiv ix e -> ix -> Maybe e
(!?) (Massiv arr) = (arr A.!?)
{-# INLINE (!?) #-}

-- | /O(1)/ - Index a Massiv. Useful with chaining of slicing
-- operations. Returns `Nothing` when index is out of bounds.
(?) :: Layout ix e => Maybe (Massiv ix e) -> ix -> Maybe e
(?) Nothing  _ = Nothing
(?) (Just m) i = m !? i
{-# INLINE (?) #-}


-- | /O(1)/ - Slices the Massiv from the left. For 2-dimensional array this will
-- be equivalent to taking a row. Throws an error when index is out of bounds.
(!>) :: (Layout (Lower ix) e, Layout ix e) =>
        Massiv ix e -> Int -> Massiv (Lower ix) e
(!>) m i = computeM (delayM m A.!> i)
{-# INLINE [~1] (!>) #-}


-- | /O(1)/ - Slices the Massiv from the left. Same as `(!>)`, except returns
-- `Nothing` when slice is impossible.
(!?>) :: (Layout (Lower ix) e, Layout ix e) =>
        Massiv ix e -> Int -> Maybe (Massiv (Lower ix) e)
(!?>) m i = fmap computeM (delayM m A.!?> i)
{-# INLINE [~1] (!?>) #-}


-- | /O(1)/ - Continue to slice a Massiv from the left.
(?>) :: (Layout (Lower ix) e, Layout ix e) =>
        Maybe (Massiv ix e) -> Int -> Maybe (Massiv (Lower ix) e)
(?>) Nothing  _ = Nothing
(?>) (Just m) i = m !?> i
{-# INLINE [~1] (?>) #-}



-- | /O(1)/ - Slices the Massiv from the left. For 2-dimensional array this will
-- be equivalent to taking a column. Throws an error when index is out of
-- bounds.
(<!) :: (Layout (Lower ix) e, Layout ix e) =>
        Massiv ix e -> Int -> Massiv (Lower ix) e
(<!) m i = computeM (delayM m A.<! i)
{-# INLINE [~1] (<!) #-}


-- | /O(1)/ - Slices the Massiv from the right. Same as `(<!)`, except returns
-- `Nothing` when slice is impossible.
(<!?) :: (Layout (Lower ix) e, Layout ix e) =>
        Massiv ix e -> Int -> Maybe (Massiv (Lower ix) e)
(<!?) m i = fmap computeM (delayM m A.<!? i)
{-# INLINE [~1] (<!?) #-}


-- | /O(1)/ - Continue to slice a Massiv from the right.
(<?) :: (Layout (Lower ix) e, Layout ix e) =>
        Maybe (Massiv ix e) -> Int -> Maybe (Massiv (Lower ix) e)
(<?) Nothing  _ = Nothing
(<?) (Just m) i = m <!? i
{-# INLINE [~1] (<?) #-}



-- | /O(1)/ - Slices the Massiv in any available dimension. Throws an error when
-- index is out of bounds or dimensions is invalid.
--
-- prop> arr !> i == arr <!> (rank (size arr), i)
-- prop> arr <! i == arr <!> (1,i)
--
(<!>) :: (Layout (Lower ix) e, Layout ix e) =>
        Massiv ix e -> (Dim, Int) -> Massiv (Lower ix) e
(<!>) m i = computeM (delayM m A.<!> i)
{-# INLINE [~1] (<!>) #-}


-- | /O(1)/ - Slices the Massiv in the middle. Same as `(<!>)`, except returns
-- `Nothing` when slice is impossible.
(<!?>) :: (Layout (Lower ix) e, Layout ix e) =>
         Massiv ix e -> (Dim, Int) -> Maybe (Massiv (Lower ix) e)
(<!?>) m i = fmap computeM (delayM m A.<!?> i)
{-# INLINE [~1] (<!?>) #-}


-- | /O(1)/ - Continue to slice a Massiv in the middle.
(<?>) :: (Layout (Lower ix) e, Layout ix e) =>
         Maybe (Massiv ix e) -> (Dim, Int) -> Maybe (Massiv (Lower ix) e)
(<?>) Nothing  _ = Nothing
(<?>) (Just m) i = m <!?> i
{-# INLINE [~1] (<?>) #-}


range :: Int -> Int -> Massiv Ix1 Int
range from = computeM . A.range Par from
{-# INLINE [~1] range #-}

rangeStep :: Int -> Int -> Int -> Massiv Ix1 Int
rangeStep from step = computeM . A.rangeStep Par from step
{-# INLINE [~1] rangeStep #-}

enumFromN :: (Num e, Layout Ix1 e) => e -> Ix1 -> Massiv Ix1 e
enumFromN from = computeM . A.enumFromN Par from
{-# INLINE [~1] enumFromN #-}

enumFromStepN :: (Num e, Layout Ix1 e) => e -> e -> Ix1 -> Massiv Ix1 e
enumFromStepN from step = computeM . A.enumFromStepN Par from step
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



-- | Convert Massiv with at least 2 dimensions into a list of lists. If higher
-- than 2D Massiv is suppliied, inner dimensions will get flattened into a list.
--
-- ==== __Examples__
--
-- >>> toListIx2 $ makeArrayIx2 Seq (2, 3) id
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toListIx2 $ makeArrayIx3 Seq (2, 1, 3) id
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toListIx2 :: (C.Slice (Repr e) ix e, Layout ix e) => Massiv ix e -> [[e]]
toListIx2 (Massiv arr) = A.toListIx2 arr
{-# INLINE [~1] toListIx2 #-}


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


product :: (Num e, Layout ix e) => Massiv ix e -> e
product = A.product . unwrapMassiv
{-# INLINE product #-}



-- | Map a function over Massiv
transpose :: Layout Ix2 e => Massiv Ix2 e -> Massiv Ix2 e
transpose = computeM . A.transpose . delayM
{-# INLINE [~1] transpose #-}


-- Stencil

mapStencil :: (Layout ix e, Layout ix a, Load DW ix a) =>
              A.Stencil ix e a -> Massiv ix e -> Massiv ix a
mapStencil stencil = computeM . A.mapStencil stencil . unwrapMassiv
{-# INLINE mapStencil #-}
