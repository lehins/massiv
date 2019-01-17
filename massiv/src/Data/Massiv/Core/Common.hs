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
-- Copyright   : (c) Alexey Kuleshevich 2018
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
  , Size(..)
  , Slice(..)
  , OuterSlice(..)
  , InnerSlice(..)
  , Manifest(..)
  , Mutable(..)
  , State(..)
  , WorldState
  , Ragged(..)
  , Nested(..)
  , NestedStruct
  , makeArray
  , singleton
  -- * Size
  , elemsCount
  , isEmpty
  -- * Indexing
  , (!?)
  , index
  , indexWith
  , (!)
  , index'
  , (??)
  , defaultIndex
  , borderIndex
  , evaluateAt
  , module Data.Massiv.Core.Index
  -- * Common Operations
  , imapM_
  , module Data.Massiv.Core.Computation
  ) where

import           Control.Monad.Primitive
import           Data.Massiv.Core.Computation
import           Data.Massiv.Core.Index
import           Data.Massiv.Core.Scheduler
import           Data.Typeable
import           GHC.Prim

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

  -- | Get computation strategy of this array
  getComp :: Array r ix e -> Comp

  -- | Set computation strategy for this array
  setComp :: Comp -> Array r ix e -> Array r ix e

  -- | Construct an array. No size validation is performed.
  unsafeMakeArray :: Comp -> ix -> (ix -> e) -> Array r ix e


-- | An array that contains size information. They can be resized and new arrays extracted from it
-- in constant time.
class Construct r ix e => Size r ix e where

  -- | /O(1)/ - Get the size of an array
  size :: Array r ix e -> Sz ix

  -- | /O(1)/ - Change the size of an array. New size is not validated.
  unsafeResize :: Index ix' => Sz ix' -> Array r ix e -> Array r ix' e

  -- | /O(1)/ - Extract a portion of an array. Staring index and new size are
  -- not validated.
  unsafeExtract :: ix -> Sz ix -> Array r ix e -> Array (EltRepr r ix) ix e


-- | Arrays that can be used as source to practically any manipulation function.
class Size r ix e => Source r ix e where

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

-- | Any array that can be computed
class Size r ix e => Load r ix e where

  -- | Load an array into memory sequentially
  loadS
    :: Monad m =>
       Array r ix e -- ^ Array that is being loaded
    -> (Int -> m e) -- ^ Function that reads an element from target array
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()
  loadS = loadArray 1 id
  {-# INLINE loadS #-}

  -- | Load an array into memory in parallel
  loadP
    :: [Int] -- ^ List of capabilities to run workers on, as described in
             -- `Control.Concurrent.forkOn`. Empty list will imply all
             -- capabilities, i.e. run on all cores available through @+RTS -N@.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> IO e) -- ^ Function that reads an element from target array
    -> (Int -> e -> IO ()) -- ^ Function that writes an element into target array
    -> IO ()
  loadP wIds arr unsafeRead unsafeWrite =
    withScheduler_ wIds $ \scheduler ->
      loadArray (numWorkers scheduler) (scheduleWork scheduler) arr unsafeRead unsafeWrite
  {-# INLINE loadP #-}

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
    -> (Int -> m e) -- ^ Function that reads an element from target array
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()
  default loadArrayWithStride
    :: (Source r ix e, Monad m) =>
       Int
    -> (m () -> m ())
    -> Stride ix
    -> Sz ix
    -> Array r ix e
    -> (Int -> m e)
    -> (Int -> e -> m ())
    -> m ()
  loadArrayWithStride numWorkers' scheduleWork' stride resultSize arr _ =
    splitLinearlyWith_ numWorkers' scheduleWork' (totalElem resultSize) unsafeLinearWriteWithStride
    where
      strideIx = unStride stride
      unsafeLinearWriteWithStride =
        unsafeIndex arr . liftIndex2 (*) strideIx . fromLinearIndex resultSize
      {-# INLINE unsafeLinearWriteWithStride #-}
  {-# INLINE loadArrayWithStride #-}

  -- TODO: this is the future replacement for loadS and loadP discussed in:
  -- https://github.com/lehins/massiv/issues/41
  -- | Load an array into memory. Default implementation will respect the scheduler and use `Source`
  -- instance to do loading in row-major fashion in parallel as well as sequentially.
  loadArray
    :: Monad m =>
       Int -- ^ Total number of workers (for `Seq` it's always 1)
    -> (m () -> m ()) -- ^ A monadic action that will schedule work for the workers (for `Seq` it's
                      -- always `id`)
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> m e) -- ^ Function that reads an element from target array
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()
  default loadArray
    :: (Source r ix e, Monad m) =>
       Int
    -> (m () -> m ())
    -> Array r ix e
    -> (Int -> m e)
    -> (Int -> e -> m ())
    -> m ()
  loadArray numWorkers' scheduleWork' arr _ =
    splitLinearlyWith_ numWorkers' scheduleWork' (totalElem (size arr)) (unsafeLinearIndex arr)
  {-# INLINE loadArray #-}

class OuterSlice r ix e where
  -- | /O(1)/ - Take a slice out of an array from the outside
  unsafeOuterSlice :: Array r ix e -> Int -> Elt r ix e

  outerLength :: Array r ix e -> Int
  default outerLength :: Size r ix e => Array r ix e -> Int
  outerLength = headDim . size

class Size r ix e => InnerSlice r ix e where
  unsafeInnerSlice :: Array r ix e -> (Sz (Lower ix), Sz1) -> Int -> Elt r ix e

class Size r ix e => Slice r ix e where
  unsafeSlice :: Array r ix e -> ix -> Sz ix -> Dim -> Maybe (Elt r ix e)


-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- @(`!`)@, @(`!?`)@, etc. are constrained to manifest arrays only.
class Source r ix e => Manifest r ix e where

  unsafeLinearIndexM :: Array r ix e -> Int -> e


data State s = State (State# s)

type WorldState = State RealWorld


class Manifest r ix e => Mutable r ix e where
  data MArray s r ix e :: *

  -- | Get the size of a mutable array.
  msize :: MArray s r ix e -> Sz ix

  unsafeThaw :: PrimMonad m =>
                Array r ix e -> m (MArray (PrimState m) r ix e)

  unsafeFreeze :: PrimMonad m =>
                  Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)

  -- | Create new mutable array, leaving it's elements uninitialized. Size isn't validated
  -- either.
  unsafeNew :: PrimMonad m =>
               Sz ix -> m (MArray (PrimState m) r ix e)

  -- | Create new mutable array, leaving it's elements uninitialized. Size isn't validated
  -- either.
  unsafeNewZero :: PrimMonad m =>
                   Sz ix -> m (MArray (PrimState m) r ix e)

  unsafeLinearRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e

  unsafeLinearWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()

  -- | Create new mutable array, leaving it's elements uninitialized. Size isn't validated
  -- either.
  unsafeNewA :: Applicative f => ix -> WorldState -> f (WorldState, MArray RealWorld r ix e)
  unsafeNewA sz (State s#) =
    case internal (unsafeNew sz :: IO (MArray RealWorld r ix e)) s# of
      (# s'#, ma #) -> pure (State s'#, ma)
  {-# INLINE unsafeNewA #-}

  unsafeThawA :: Applicative m =>
                 Array r ix e -> WorldState -> m (WorldState, MArray RealWorld r ix e)
  unsafeThawA arr (State s#) =
    case internal (unsafeThaw arr :: IO (MArray RealWorld r ix e)) s# of
      (# s'#, ma #) -> pure (State s'#, ma)
  {-# INLINE unsafeThawA #-}

  unsafeFreezeA :: Applicative m =>
                   Comp -> MArray RealWorld r ix e -> WorldState -> m (WorldState, Array r ix e)
  unsafeFreezeA comp marr (State s#) =
    case internal (unsafeFreeze comp marr :: IO (Array r ix e)) s# of
      (# s'#, a #) -> pure (State s'#, a)
  {-# INLINE unsafeFreezeA #-}

  unsafeLinearWriteA :: Applicative m =>
                        MArray RealWorld r ix e -> Int -> e -> WorldState -> m WorldState
  unsafeLinearWriteA marr i val (State s#) =
    case internal (unsafeLinearWrite marr i val :: IO ()) s# of
      (# s'#, _ #) -> pure (State s'#)
  {-# INLINE unsafeLinearWriteA #-}



class Nested r ix e where
  fromNested :: NestedStruct r ix e -> Array r ix e

  toNested :: Array r ix e -> NestedStruct r ix e


class Construct r ix e => Ragged r ix e where

  empty :: Comp -> Array r ix e

  isNull :: Array r ix e -> Bool

  cons :: Elt r ix e -> Array r ix e -> Array r ix e

  uncons :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  -- head :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  -- tail :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  unsafeGenerateM :: Monad m => Comp -> Sz ix -> (ix -> m e) -> m (Array r ix e)

  edgeSize :: Array r ix e -> Sz ix

  flatten :: Array r ix e -> Array r Ix1 e

  loadRagged ::
    (IO () -> IO ()) -> (Int -> e -> IO a) -> Int -> Int -> ix -> Array r ix e -> IO ()

  -- TODO: test property:
  -- (read $ raggedFormat show "\n" (ls :: Array L (IxN n) Int)) == ls
  raggedFormat :: (e -> String) -> String -> Array r ix e -> String



-- | Create an Array. Resulting type either has to be unambiguously inferred or restricted manually,
-- like in the example below.
--
-- >>> makeArray Seq (3 :. 4) (\ (i :. j) -> if i == j then i else 0) :: Array D Ix2 Int
-- (Array D Seq (3 :. 4)
-- [ [ 0,0,0,0 ]
-- , [ 0,1,0,0 ]
-- , [ 0,0,2,0 ]
-- ])
--
makeArray :: Construct r ix e =>
             Comp -- ^ Computation strategy. Useful constructors are `Seq` and `Par`
          -> Sz ix -- ^ Size of the result array. Negative values will result in an empty array.
          -> (ix -> e) -- ^ Function to generate elements at a particular index
          -> Array r ix e
makeArray !c = unsafeMakeArray c . liftIndex (max 0)
{-# INLINE makeArray #-}


-- | Create an Array with a single element.
singleton :: Construct r ix e =>
             Comp -- ^ Computation strategy
          -> e -- ^ The element
          -> Array r ix e
singleton !c = unsafeMakeArray c (pureIndex 1) . const
{-# INLINE singleton #-}


infixl 4 !, !?, ??

-- | Infix version of `index'`.
(!) :: Manifest r ix e => Array r ix e -> ix -> e
(!) = index'
{-# INLINE (!) #-}


-- | Infix version of `index`.
(!?) :: Manifest r ix e => Array r ix e -> ix -> Maybe e
(!?) = index
{-# INLINE (!?) #-}


-- | /O(1)/ - Lookup an element in the array, where array can itself be
-- `Nothing`. This operator is useful when used together with slicing or other
-- functions that return `Maybe` array:
--
-- >>> (fromList Seq [[[1,2,3]],[[4,5,6]]] :: Maybe (Array U Ix3 Int)) ??> 1 ?? (0 :. 2)
-- Just 6
--
(??) :: Manifest r ix e => Maybe (Array r ix e) -> ix -> Maybe e
(??) Nothing    = const Nothing
(??) (Just arr) = (arr !?)
{-# INLINE (??) #-}

-- | /O(1)/ - Lookup an element in the array. Returns `Nothing`, when index is out
-- of bounds, `Just` element otherwise.
index :: Manifest r ix e => Array r ix e -> ix -> Maybe e
index arr = handleBorderIndex (Fill Nothing) (size arr) (Just . unsafeIndex arr)
{-# INLINE index #-}

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

-- | /O(1)/ - Lookup an element in the array. Throw an error if index is out of bounds.
index' :: Manifest r ix e => Array r ix e -> ix -> e
index' arr ix =
  borderIndex (Fill (errorIx "Data.Massiv.Array.index" (size arr) ix)) arr ix
{-# INLINE index' #-}


-- | This is just like `index'` function, but it allows getting values from
-- delayed arrays as well as manifest. As the name suggests, indexing into a
-- delayed array at the same index multiple times will cause evaluation of the
-- value each time and can destroy the performace if used without care.
evaluateAt :: Source r ix e => Array r ix e -> ix -> e
evaluateAt !arr !ix =
  handleBorderIndex
    (Fill (errorIx "Data.Massiv.Array.evaluateAt" (size arr) ix))
    (size arr)
    (unsafeIndex arr)
    ix
{-# INLINE evaluateAt #-}


indexWith ::
     Index ix
  => String -- ^ Source file name, eg. __FILE__
  -> Int -- ^ Line number in th source file, eg. __LINE__
  -> String
  -> (arr -> ix) -- ^ Get size of the array
  -> (arr -> ix -> e) -- ^ Indexing function
  -> arr -- ^ Array
  -> ix -- ^ Index
  -> e
indexWith fileName lineNo funName getSize f arr ix
  | isSafeIndex (getSize arr) ix = f arr ix
  | otherwise = errorIx ("<" ++ fileName ++ ":" ++ show lineNo ++ "> " ++ funName) (getSize arr) ix
{-# INLINE indexWith #-}


-- indexWith :: Size r ix e => String -> Int -> (Array r ix e -> ix -> e) -> Array r ix e -> ix -> e
-- indexWith fileName lineNo f arr ix
--   | isSafeIndex (size arr) ix = f arr ix
--   | otherwise = errorIx ("<" ++ fileName ++ ":" ++ show lineNo ++ "> indexWith") (size arr) ix
-- {-# INLINE indexWith #-}

-- errorImpossible :: String -> a
-- errorImpossible loc =
--   error $ "Please report this error. Impossible happend at: " ++ loc
-- {-# NOINLINE errorImpossible #-}



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
  iterM_ zeroIndex (size arr) (pureIndex 1) (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}


-- | /O(1)/ - Get the number of elements in the array
elemsCount :: Size r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array has no elements.
isEmpty :: Size r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}
