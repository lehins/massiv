{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
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
  , Ragged(..)
  , Nested(..)
  , NestedStruct
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
  , module Data.Massiv.Scheduler.Computation
  , Semigroup((<>))
  -- * Exceptions
  , MonadThrow(..)
  , throw
  , IndexException(..)
  , SizeException(..)
  , ShapeException(..)
  , module Data.Massiv.Core.Exception
  ) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Control.Exception (throw)
import           Control.Monad.Catch (MonadThrow(..))
import           Control.Monad.Primitive
import           Data.Massiv.Scheduler.Computation
import           Data.Massiv.Core.Exception
import           Data.Massiv.Core.Index
import           Data.Massiv.Core.Index.Internal
import           Data.Typeable

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
  setComp :: Comp -> Array r ix e -> Array r ix e

  -- | Construct an Array. Resulting type either has to be unambiguously inferred or restricted
  -- manually, like in the example below. Use "Data.Massiv.Array.makeArrayR" if you'd like to
  -- specify representation as an argument.
  --
  -- >>> makeArray Seq (3 :. 4) (\ (i :. j) -> if i == j then i else 0) :: Array D Ix2 Int
  -- (Array D Seq (3 :. 4)
  -- [ [ 0,0,0,0 ]
  -- , [ 0,1,0,0 ]
  -- , [ 0,0,2,0 ]
  -- ])
  --
  makeArray :: Comp -- ^ Computation strategy. Useful constructors are `Seq` and `Par`
            -> Sz ix -- ^ Size of the result array.
            -> (ix -> e) -- ^ Function to generate elements at a particular index
            -> Array r ix e
  makeArray comp sz f = makeArrayLinear comp sz (f . fromLinearIndex sz)
  {-# INLINE makeArray #-}

  -- | Same as `makeArray`, but produce elements using linear row-major index.
  makeArrayLinear :: Comp -> Sz ix -> (Int -> e) -> Array r ix e
  makeArrayLinear comp sz f = makeArray comp sz (f . toLinearIndex sz)
  {-# INLINE makeArrayLinear #-}



class Index ix => Resize array r ix where
  -- | /O(1)/ - Change the size of an array. Total number of elements should be the same, but it is
  -- not validated.
  unsafeResize :: Index ix' => Sz ix' -> array r ix e -> array r ix' e


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

  -- | Load an array into memory. Default implementation will respect the scheduler and use `Source`
  -- instance to do loading in row-major fashion in parallel as well as sequentially.
  loadArray
    :: Monad m =>
       Int -- ^ Total number of workers (for `Seq` it's always 1)
    -> (m () -> m ()) -- ^ A monadic action that will schedule work for the workers (for `Seq` it's
                      -- always `id`)
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()
  default loadArray
    :: (Source r ix e, Monad m) =>
       Int
    -> (m () -> m ())
    -> Array r ix e
    -> (Int -> e -> m ())
    -> m ()
  loadArray numWorkers scheduleWork arr =
    splitLinearlyWith_ numWorkers scheduleWork (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArray #-}

class Load r ix e => StrideLoad r ix e where
  -- | Load an array into memory with stride. Default implementation can only handle the sequential
  -- case and only if there is an instance of `Source`.
  loadArrayWithStride
    :: Monad m =>
       Int -- ^ Total number of workers (for `Seq` it's always 1)
    -> (m () -> m ()) -- ^ A monadic action that will schedule work for the workers (for `Seq` it's
                      -- always `id`)
    -> Stride ix -- ^ Stride to use
    -> Sz ix -- ^ Size of the target array affected by the stride.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()
  default loadArrayWithStride
    :: (Source r ix e, Monad m) =>
       Int
    -> (m () -> m ())
    -> Stride ix
    -> Sz ix
    -> Array r ix e
    -> (Int -> e -> m ())
    -> m ()
  loadArrayWithStride numWorkers' scheduleWork' stride resultSize arr =
    splitLinearlyWith_ numWorkers' scheduleWork' (totalElem resultSize) unsafeLinearWriteWithStride
    where
      strideIx = unStride stride
      unsafeLinearWriteWithStride =
        unsafeIndex arr . liftIndex2 (*) strideIx . fromLinearIndex resultSize
      {-# INLINE unsafeLinearWriteWithStride #-}
  {-# INLINE loadArrayWithStride #-}


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
  unsafeThaw :: PrimMonad m =>
                Array r ix e -> m (MArray (PrimState m) r ix e)

  -- | Convert mutable array into an immutable array without copy.
  --
  -- @since 0.1.0
  unsafeFreeze :: PrimMonad m =>
                  Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)

  -- | Create new mutable array, leaving it's elements uninitialized. Size isn't validated
  -- either.
  --
  -- @since 0.1.0
  unsafeNew :: PrimMonad m =>
               Sz ix -> m (MArray (PrimState m) r ix e)

  -- | Read an element at linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e

  -- | Write an element into mutable array with linear row-major index
  --
  -- @since 0.1.0
  unsafeLinearWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()

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
      Nothing -> initialize marr
    return marr
  {-# INLINE initializeNew #-}


  unsafeLinearSet :: PrimMonad m =>
                     MArray (PrimState m) r ix e -> Int -> Int -> e -> m ()
  unsafeLinearSet marr offset len e =
    loopM_ offset (< (offset + len)) (+1) (\i -> unsafeLinearWrite marr i e)
  {-# INLINE unsafeLinearSet #-}



class Nested r ix e where
  fromNested :: NestedStruct r ix e -> Array r ix e

  toNested :: Array r ix e -> NestedStruct r ix e


class Construct r ix e => Ragged r ix e where

  empty :: Comp -> Array r ix e

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



-- | Create an Array with a single element.
singleton :: Construct r ix e =>
             Comp -- ^ Computation strategy
          -> e -- ^ The element
          -> Array r ix e
singleton !c = makeArray c oneSz . const
{-# INLINE singleton #-}


infixl 4 !, !?, ??

-- | Infix version of `index'`.
(!) :: Manifest r ix e => Array r ix e -> ix -> e
(!) = index'
{-# INLINE (!) #-}


-- | Infix version of `index`.
(!?) :: (MonadThrow m, Manifest r ix e) => Array r ix e -> ix -> m e
(!?) = indexM
{-# INLINE (!?) #-}


-- | /O(1)/ - Lookup an element in the array, where array can itself be
-- `Nothing`. This operator is useful when used together with slicing or other
-- functions that return `Maybe` array:
--
-- >>> (fromList Seq [[[1,2,3]],[[4,5,6]]] :: Maybe (Array U Ix3 Int)) ??> 1 ?? (0 :. 2)
-- Just 6
--
(??) :: (MonadThrow m, Manifest r ix e) => m (Array r ix e) -> ix -> m e
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
indexM :: (MonadThrow m, Manifest r ix e) => Array r ix e -> ix -> m e
indexM = evaluateM
{-# INLINE indexM #-}

-- | /O(1)/ - Lookup an element in the array, while using default element when
-- index is out of bounds.
defaultIndex :: Manifest r ix e => e -> Array r ix e -> ix -> e
defaultIndex defVal = borderIndex (Fill defVal)
{-# INLINE defaultIndex #-}

-- | /O(1)/ - Lookup an element in the array. Use a border resolution technique
-- when index is out of bounds.
borderIndex :: Manifest r ix e => Border e -> Array r ix e -> ix -> e
borderIndex border arr = handleBorderIndex border (size arr) (unsafeIndex arr)
{-# INLINE borderIndex #-}

-- | /O(1)/ - Lookup an element in the array. This is a partial function and it can throw
-- `IndexOutOfBoundsException` inside pure code. It is safer to use `index` instead.
index' :: Manifest r ix e => Array r ix e -> ix -> e
index' = evaluate'
{-# INLINE index' #-}

-- | This is just like `indexM` function, but it allows getting values from
-- delayed arrays as well as `Manifest`. As the name suggests, indexing into a
-- delayed array at the same index multiple times will cause evaluation of the
-- value each time and can destroy the performace if used without care.
--
-- @since 0.3.0
evaluateM :: (MonadThrow m, Source r ix e) => Array r ix e -> ix -> m e
evaluateM arr ix =
  handleBorderIndex
    (Fill (throwM (IndexOutOfBoundsException (size arr) ix)))
    (size arr)
    (pure . unsafeIndex arr)
    ix
{-# INLINE evaluateM #-}

-- | Similar to `evaluateM`, but will throw an exception in pure code.
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
{-# INLINE indexWith #-}



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
imapM_ :: (Source r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f !arr =
  iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}


-- | /O(1)/ - Get the number of elements in the array
elemsCount :: Load r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array has no elements.
isEmpty :: Load r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}
