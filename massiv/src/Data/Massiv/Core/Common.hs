{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Core.Common
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.Common
  ( Array
  , Elt
  , EltRepr
  , Construct(..)
  , Source(..)
  , Load(..)
  , StrideLoad(..)
  , Resize(..)
  , Extract(..)
  , Slice(..)
  , OuterSlice(..)
  , InnerSlice(..)
  , Manifest(..)
  , Mutable(..)
  , Comp(..)
  , Scheduler(..)
  , unsafeRead
  , unsafeWrite
  , unsafeLinearModify
  , Ragged(..)
  , Nested(..)
  , NestedStruct
  , empty
  , singleton
  -- * Size
  , elemsCount
  , isEmpty
  -- * Indexing
  , (!?)
  , index
  , indexM
  , indexWith
  , (!)
  , index'
  , (??)
  , defaultIndex
  , borderIndex
  , evaluateM
  , evaluate'
  , evaluateAt
  , module Data.Massiv.Core.Index
  -- * Common Operations
  , imapM_
  , Semigroup((<>))
  -- * Exceptions
  , MonadThrow(..)
  , throw
  , IndexException(..)
  , SizeException(..)
  , ShapeException(..)
  , module Data.Massiv.Core.Exception
  -- * Stateful Monads
  , MonadUnliftIO
  , MonadIO(liftIO)
  , PrimMonad(PrimState)
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Control.Exception (throw)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Unlift (MonadIO(liftIO), MonadUnliftIO)
import Control.Monad.Primitive
import Control.Scheduler (Comp(..), Scheduler(..))
import Data.Massiv.Core.Exception
import Data.Massiv.Core.Index
import Data.Typeable

#include "massiv.h"

-- | The array family. Representations @r@ describes how data is arranged or computed. All arrays
-- have a common property that each index @ix@ always maps to the same unique element, even if that
-- element does not exist in memory and has to be computed upon lookup. Data is always arranged in a
-- nested fasion, depth of which is controlled by @`Rank` ix@.
data family Array r ix e :: *

type family EltRepr r ix :: *

type family Elt r ix e :: * where
  Elt r Ix1 e = e
  Elt r ix  e = Array (EltRepr r ix) (Lower ix) e

type family NestedStruct r ix e :: *

-- | Array types that can be constructed.
class (Typeable r, Index ix) => Construct r ix e where
  {-# MINIMAL setComp,(makeArray|makeArrayLinear) #-}

  -- | Set computation strategy for this array
  --
  -- ==== __Example__
  --
  -- >>> :set -XTypeApplications
  -- >>> import Data.Massiv.Array
  -- >>> a = singleton @DL @Ix1 @Int 0
  -- >>> a
  -- Array DL Seq (Sz1 1)
  --   [ 0 ]
  -- >>> setComp (ParN 6) a -- use 6 capabilities
  -- Array DL (ParN 6) (Sz1 1)
  --   [ 0 ]
  --
  setComp :: Comp -> Array r ix e -> Array r ix e

  -- | Construct an Array. Resulting type either has to be unambiguously inferred or restricted
  -- manually, like in the example below. Use "Data.Massiv.Array.makeArrayR" if you'd like to
  -- specify representation as an argument.
  --
  -- >>> import Data.Massiv.Array
  -- >>> makeArray Seq (Sz (3 :. 4)) (\ (i :. j) -> if i == j then i else 0) :: Array D Ix2 Int
  -- Array D Seq (Sz (3 :. 4))
  --   [ [ 0, 0, 0, 0 ]
  --   , [ 0, 1, 0, 0 ]
  --   , [ 0, 0, 2, 0 ]
  --   ]
  --
  -- Instead of restricting the full type manually we can use `TypeApplications` as convenience:
  --
  -- >>> :set -XTypeApplications
  -- >>> makeArray @P @_ @Double Seq (Sz2 3 4) $ \(i :. j) -> logBase (fromIntegral i) (fromIntegral j)
  -- Array P Seq (Sz (3 :. 4))
  --   [ [ NaN, -0.0, -0.0, -0.0 ]
  --   , [ -Infinity, NaN, Infinity, Infinity ]
  --   , [ -Infinity, 0.0, 1.0, 1.5849625007211563 ]
  --   ]
  --
  -- @since 0.1.0
  makeArray ::
       Comp -- ^ Computation strategy. Useful constructors are `Seq` and `Par`
    -> Sz ix -- ^ Size of the result array.
    -> (ix -> e) -- ^ Function to generate elements at a particular index
    -> Array r ix e
  makeArray comp sz f = makeArrayLinear comp sz (f . fromLinearIndex sz)
  {-# INLINE makeArray #-}

  -- | Same as `makeArray`, but produce elements using linear row-major index.
  --
  -- >>> import Data.Massiv.Array
  -- >>> makeArrayLinear Seq (Sz (2 :. 4)) id :: Array D Ix2 Int
  -- Array D Seq (Sz (2 :. 4))
  --   [ [ 0, 1, 2, 3 ]
  --   , [ 4, 5, 6, 7 ]
  --   ]
  --
  -- @since 0.3.0
  makeArrayLinear :: Comp -> Sz ix -> (Int -> e) -> Array r ix e
  makeArrayLinear comp sz f = makeArray comp sz (f . toLinearIndex sz)
  {-# INLINE makeArrayLinear #-}



class Index ix => Resize r ix where
  -- | /O(1)/ - Change the size of an array. Total number of elements should be the same, but it is
  -- not validated.
  unsafeResize :: Index ix' => Sz ix' -> Array r ix e -> Array r ix' e


class Load r ix e => Extract r ix e where
  -- | /O(1)/ - Extract a portion of an array. Staring index and new size are
  -- not validated.
  unsafeExtract :: ix -> Sz ix -> Array r ix e -> Array (EltRepr r ix) ix e


-- | Arrays that can be used as source to practically any manipulation function.
class Load r ix e => Source r ix e where
  {-# MINIMAL (unsafeIndex|unsafeLinearIndex) #-}

  -- | Lookup element in the array. No bounds check is performed and access of
  -- arbitrary memory is possible when invalid index is supplied.
  unsafeIndex :: Array r ix e -> ix -> e
  unsafeIndex =
    INDEX_CHECK("(Source r ix e).unsafeIndex",
                size, \ !arr -> unsafeLinearIndex arr . toLinearIndex (size arr))
  {-# INLINE unsafeIndex #-}

  -- | Lookup element in the array using flat index in a row-major fasion. No
  -- bounds check is performed
  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}

-- | Any array that can be computed and loaded into memory
class (Typeable r, Index ix) => Load r ix e where

  -- | Get computation strategy of this array

  getComp :: Array r ix e -> Comp

  -- | Get the size of an immutabe array
  size :: Array r ix e -> Sz ix

  -- | Load an array into memory.
  loadArrayM
    :: Monad m =>
       Scheduler m ()
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()

class Load r ix e => StrideLoad r ix e where
  -- | Load an array into memory with stride. Default implementation requires an instance of
  -- `Source`.
  loadArrayWithStrideM
    :: Monad m =>
       Scheduler m ()
    -> Stride ix -- ^ Stride to use
    -> Sz ix -- ^ Size of the target array affected by the stride.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()
  default loadArrayWithStrideM
    :: (Source r ix e, Monad m) =>
       Scheduler m ()
    -> Stride ix
    -> Sz ix
    -> Array r ix e
    -> (Int -> e -> m ())
    -> m ()
  loadArrayWithStrideM scheduler stride resultSize arr =
    splitLinearlyWith_ scheduler (totalElem resultSize) unsafeLinearWriteWithStride
    where
      !strideIx = unStride stride
      unsafeLinearWriteWithStride =
        unsafeIndex arr . liftIndex2 (*) strideIx . fromLinearIndex resultSize
      {-# INLINE unsafeLinearWriteWithStride #-}
  {-# INLINE loadArrayWithStrideM #-}


class Load r ix e => OuterSlice r ix e where
  -- | /O(1)/ - Take a slice out of an array from the outside
  unsafeOuterSlice :: Array r ix e -> Int -> Elt r ix e

class Load r ix e => InnerSlice r ix e where
  unsafeInnerSlice :: Array r ix e -> (Sz (Lower ix), Sz Int) -> Int -> Elt r ix e

class Load r ix e => Slice r ix e where
  unsafeSlice :: MonadThrow m => Array r ix e -> ix -> Sz ix -> Dim -> m (Elt r ix e)


-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- @(`!`)@, @(`!?`)@, etc. are constrained to manifest arrays only.
class (Load r ix e, Source r ix e) => Manifest r ix e where

  unsafeLinearIndexM :: Array r ix e -> Int -> e


class Manifest r ix e => Mutable r ix e where
  data MArray s r ix e :: *

  -- | Get the size of a mutable array.
  --
  -- @since 0.1.0
  msize :: MArray s r ix e -> Sz ix

  -- | Convert immutable array into a mutable array without copy.
  --
  -- @since 0.1.0
  unsafeThaw :: PrimMonad m => Array r ix e -> m (MArray (PrimState m) r ix e)

  -- | Convert mutable array into an immutable array without copy.
  --
  -- @since 0.1.0
  unsafeFreeze :: PrimMonad m => Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)

  -- | Create new mutable array, leaving it's elements uninitialized. Size isn't validated either.
  --
  -- @since 0.1.0
  unsafeNew :: PrimMonad m => Sz ix -> m (MArray (PrimState m) r ix e)

  -- | Read an element at linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearRead :: PrimMonad m => MArray (PrimState m) r ix e -> Int -> m e

  -- | Write an element into mutable array with linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearWrite :: PrimMonad m => MArray (PrimState m) r ix e -> Int -> e -> m ()

  -- | Initialize mutable array to some default value.
  --
  -- @since 0.3.0
  initialize :: PrimMonad m => MArray (PrimState m) r ix e -> m ()

  -- | Create new mutable array while initializing all elements to some default value.
  --
  -- @since 0.3.0
  initializeNew :: PrimMonad m =>
                   Maybe e -> Sz ix -> m (MArray (PrimState m) r ix e)
  initializeNew mdef sz = do
    marr <- unsafeNew sz
    case mdef of
      Just val -> unsafeLinearSet marr 0 (totalElem sz) val
      Nothing  -> initialize marr
    return marr
  {-# INLINE initializeNew #-}


  unsafeLinearSet :: PrimMonad m =>
                     MArray (PrimState m) r ix e -> Int -> Int -> e -> m ()
  unsafeLinearSet marr offset len e =
    loopM_ offset (< (offset + len)) (+1) (\i -> unsafeLinearWrite marr i e)
  {-# INLINE unsafeLinearSet #-}

-- | Read an array element
unsafeRead :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> m e
unsafeRead !marr !ix = unsafeLinearRead marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeRead #-}

-- | Write an element into array
unsafeWrite :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> e -> m ()
unsafeWrite !marr !ix = unsafeLinearWrite marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeWrite #-}


-- | Modify an element in the array with an index aware action.
unsafeLinearModify :: (Mutable r ix e, PrimMonad m) =>
                      MArray (PrimState m) r ix e -> (Int -> e -> m e) -> Int -> m ()
unsafeLinearModify !marr f !i = do
  v <- unsafeLinearRead marr i
  v' <- f i v
  unsafeLinearWrite marr i v'
{-# INLINE unsafeLinearModify #-}


class Nested r ix e where
  fromNested :: NestedStruct r ix e -> Array r ix e

  toNested :: Array r ix e -> NestedStruct r ix e


class Construct r ix e => Ragged r ix e where

  emptyR :: Comp -> Array r ix e

  isNull :: Array r ix e -> Bool

  consR :: Elt r ix e -> Array r ix e -> Array r ix e

  unconsR :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  generateRaggedM :: Monad m => Comp -> Sz ix -> (ix -> m e) -> m (Array r ix e)

  edgeSize :: Array r ix e -> Sz ix

  flatten :: Array r ix e -> Array r Ix1 e

  loadRagged ::
    Monad m => (m () -> m ()) -> (Int -> e -> m a) -> Int -> Int -> Sz ix -> Array r ix e -> m ()

  -- TODO: test property:
  -- (read $ raggedFormat show "\n" (ls :: Array L (IxN n) Int)) == ls
  raggedFormat :: (e -> String) -> String -> Array r ix e -> String



-- | Create an Array with no elements. By itself it is not particularly useful, but it serves as a
-- nice base for constructing larger arrays.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XTypeApplications
-- >>> xs = empty @DL @Ix1 @Double
-- >>> snoc (cons 4 (cons 5 xs)) 22
-- Array DL Seq (Sz1 3)
--   [ 4.0, 5.0, 22.0 ]
--
-- @since 0.3.0
empty ::
     forall r ix e. Construct r ix e
  => Array r ix e
empty = makeArray Seq zeroSz (const (throwImpossible Uninitialized))
{-# INLINE empty #-}

-- | Create an Array with a single element.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> singleton 7 :: Array D Ix4 Double
-- Array D Seq (Sz (1 :> 1 :> 1 :. 1))
--   [ [ [ [ 7.0 ]
--       ]
--     ]
--   ]
--
-- Instead of specifying type signature we could use @TypeApplications@
--
-- >>> :set -XTypeApplications
-- >>> singleton @U @Ix4 @Double 7
-- Array U Seq (Sz (1 :> 1 :> 1 :. 1))
--   [ [ [ [ 7.0 ]
--       ]
--     ]
--   ]
--
-- @since 0.1.0
singleton ::
     forall r ix e. Construct r ix e
  => e -- ^ The only element
  -> Array r ix e
singleton = makeArray Seq oneSz . const
{-# INLINE singleton #-}


infixl 4 !, !?, ??

-- | Infix version of `index'`.
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs U $ iterateN Seq (Sz (2 :. 3)) succ (0 :: Int)
-- >>> a
-- Array U Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
-- >>> a ! 0 :. 2
-- 3
-- >>> a ! 0 :. 3
-- *** Exception: IndexOutOfBoundsException: (0 :. 3) not safe for (Sz (2 :. 3))
--
-- @since 0.1.0
(!) :: Manifest r ix e => Array r ix e -> ix -> e
(!) = index'
{-# INLINE (!) #-}


-- | Infix version of `index`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XTypeApplications
-- >>> a <- fromListsM @U @Ix2 @Int Seq [[1,2,3],[4,5,6]]
-- >>> a
-- Array U Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
-- >>> a !? 0 :. 2
-- 3
-- >>> a !? 0 :. 3
-- *** Exception: IndexOutOfBoundsException: (0 :. 3) not safe for (Sz (2 :. 3))
-- >>> a !? 0 :. 3 :: Maybe Int
-- Nothing
--
-- @since 0.1.0
(!?) :: (Manifest r ix e, MonadThrow m) => Array r ix e -> ix -> m e
(!?) = indexM
{-# INLINE (!?) #-}


-- | /O(1)/ - Lookup an element in the array, where array itself is wrapped with
-- `MonadThrow`. This operator is useful when used together with slicing or other
-- functions that can fail.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XTypeApplications
-- >>> ma = fromListsM @U @Ix3 @Int @Maybe Seq [[[1,2,3]],[[4,5,6]]]
-- >>> ma
-- Just (Array U Seq (Sz (2 :> 1 :. 3))
--   [ [ [ 1, 2, 3 ]
--     ]
--   , [ [ 4, 5, 6 ]
--     ]
--   ]
-- )
-- >>> ma ??> 1
-- Just (Array M Seq (Sz (1 :. 3))
--   [ [ 4, 5, 6 ]
--   ]
-- )
-- >>> ma ??> 1 ?? 0 :. 2
-- Just 6
-- >>> ma ?? 1 :> 0 :. 2
-- Just 6
--
-- @since 0.1.0
(??) :: (Manifest r ix e, MonadThrow m) => m (Array r ix e) -> ix -> m e
(??) marr ix = marr >>= (!? ix)
{-# INLINE (??) #-}

-- | /O(1)/ - Lookup an element in the array. Returns `Nothing`, when index is out of bounds and
-- returns the element at the supplied index otherwise. Use `indexM` instead, since it is more
-- generaland can just as well be used with `Maybe`.
--
-- @since 0.1.0
index :: Manifest r ix e => Array r ix e -> ix -> Maybe e
index = indexM
{-# INLINE index #-}

-- | /O(1)/ - Lookup an element in the array. Throws `IndexOutOfBoundsException`, when index is out
-- of bounds and returns the element at the supplied index otherwise.
--
-- @since 0.3.0
indexM :: (Manifest r ix e, MonadThrow m) => Array r ix e -> ix -> m e
indexM = evaluateM
{-# INLINE indexM #-}

-- | /O(1)/ - Lookup an element in the array, while using default element when index is out of
-- bounds.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XOverloadedLists
-- >>> xs = [0..100] :: Array P Ix1 Int
-- >>> defaultIndex 999 xs 100
-- 100
-- >>> defaultIndex 999 xs 101
-- 999
--
-- @since 0.1.0
defaultIndex :: Manifest r ix e => e -> Array r ix e -> ix -> e
defaultIndex defVal = borderIndex (Fill defVal)
{-# INLINE defaultIndex #-}

-- | /O(1)/ - Lookup an element in the array. Use a border resolution technique
-- when index is out of bounds.
--
-- >>> import Data.Massiv.Array as A
-- >>> :set -XOverloadedLists
-- >>> xs = [0..100] :: Array U Ix1 Int
-- >>> borderIndex Wrap xs <$> range Seq 99 104
-- Array D Seq (Sz1 5)
--   [ 99, 100, 0, 1, 2 ]
--
-- @since 0.1.0
borderIndex :: Manifest r ix e => Border e -> Array r ix e -> ix -> e
borderIndex border arr = handleBorderIndex border (size arr) (unsafeIndex arr)
{-# INLINE borderIndex #-}

-- | /O(1)/ - Lookup an element in the array. This is a partial function and it can throw
-- `IndexOutOfBoundsException` inside pure code. It is safer to use `index` instead.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XOverloadedLists
-- >>> xs = [0..100] :: Array U Ix1 Int
-- >>> index' xs 50
-- 50
-- >>> index' xs 150
-- *** Exception: IndexOutOfBoundsException: 150 not safe for (Sz1 101)
--
-- @since 0.1.0
index' :: Manifest r ix e => Array r ix e -> ix -> e
index' = evaluate'
{-# INLINE index' #-}

-- | This is just like `indexM` function, but it allows getting values from
-- delayed arrays as well as `Manifest`. As the name suggests, indexing into a
-- delayed array at the same index multiple times will cause evaluation of the
-- value each time and can destroy the performace if used without care.
--
-- ==== __Examples__
--
-- >>> import Control.Exception
-- >>> import Data.Massiv.Array
-- >>> evaluateM (range Seq (Ix2 10 20) (100 :. 210)) 50 :: Either SomeException Ix2
-- Right (60 :. 70)
-- >>> evaluateM (range Seq (Ix2 10 20) (100 :. 210)) 150 :: Either SomeException Ix2
-- Left (IndexOutOfBoundsException: (150 :. 150) not safe for (Sz (90 :. 190)))
--
-- @since 0.3.0
evaluateM :: (Source r ix e, MonadThrow m) => Array r ix e -> ix -> m e
evaluateM arr ix =
  handleBorderIndex
    (Fill (throwM (IndexOutOfBoundsException (size arr) ix)))
    (size arr)
    (pure . unsafeIndex arr)
    ix
{-# INLINE evaluateM #-}

-- | Similar to `evaluateM`, but will throw an exception in pure code.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> evaluate' (range Seq (Ix2 10 20) (100 :. 210)) 50
-- 60 :. 70
-- >>> evaluate' (range Seq (Ix2 10 20) (100 :. 210)) 150
-- *** Exception: IndexOutOfBoundsException: (150 :. 150) not safe for (Sz (90 :. 190))
--
-- @since 0.3.0
evaluate' :: Source r ix e => Array r ix e -> ix -> e
evaluate' arr ix =
  handleBorderIndex
    (Fill (throw (IndexOutOfBoundsException (size arr) ix)))
    (size arr)
    (unsafeIndex arr)
    ix
{-# INLINE evaluate' #-}

-- | See `evaluate'`.
--
-- @since 0.1.0
evaluateAt :: Source r ix e => Array r ix e -> ix -> e
evaluateAt = evaluate'
{-# INLINE evaluateAt #-}
{-# DEPRECATED evaluateAt "In favor of a safe `evaluateM` or an equivalent `evaluate'`" #-}


-- | This is only used together with the @unsafe-checks@ cabal flag
indexWith ::
     Index ix
  => String -- ^ Source file name, eg. __FILE__
  -> Int -- ^ Line number in th source file, eg. __LINE__
  -> String
  -> (arr -> Sz ix) -- ^ Get size of the array
  -> (arr -> ix -> e) -- ^ Indexing function
  -> arr -- ^ Array
  -> ix -- ^ Index
  -> e
indexWith fileName lineNo funName getSize' f arr ix
  | isSafeIndex (getSize' arr) ix = f arr ix
  | otherwise = errorIx ("<" ++ fileName ++ ":" ++ show lineNo ++ "> " ++ funName) (getSize' arr) ix
{-# NOINLINE indexWith #-}

-- | Helper function for throwing out of bounds error. Used by `indexWith`
errorIx :: (Show ix, Show ix') => String -> ix -> ix' -> a
errorIx fName sz ix =
  error $
  fName ++
  ": Index out of bounds: (" ++ show ix ++ ") for Array of size: (" ++ show sz ++ ")"
{-# NOINLINE errorIx #-}


-- | Map a monadic index aware function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> imapM_ (curry print) $ range Seq (Ix1 10) 15
-- (0,10)
-- (1,11)
-- (2,12)
-- (3,13)
-- (4,14)
--
-- @since 0.1.0
imapM_ :: (Source r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f !arr =
  iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}


-- | /O(1)/ - Get the number of elements in the array
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> elemsCount $ range Seq (Ix1 10) 15
-- 5
--
-- @since 0.1.0
elemsCount :: Load r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array has no elements.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> isEmpty $ range Seq (Ix2 10 20) (11 :. 21)
-- False
-- >>> isEmpty $ range Seq (Ix2 10 20) (10 :. 21)
-- True
--
-- @since 0.1.0
isEmpty :: Load r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}
