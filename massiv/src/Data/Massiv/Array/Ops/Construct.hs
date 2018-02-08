{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Construct
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Construct
  ( makeArray
  , makeArrayR
  , singleton
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  ) where

import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Core.Common
import           Prelude                            as P


-- | Just like `makeArray` but with ability to specify the result representation. Note the `U`nboxed
-- type constructor in the below example.
--
-- >>> makeArrayR U Par (2 :> 3 :. 4) (\ (i :> j :. k) -> i * i + j * j == k * k)
-- (Array U Par (2 :> 3 :. 4)
-- [ [ [ True,False,False,False ]
--   , [ False,True,False,False ]
--   , [ False,False,True,False ]
--   ]
-- , [ [ False,True,False,False ]
--   , [ False,False,False,False ]
--   , [ False,False,False,False ]
--   ]
-- ])
--
makeArrayR :: Construct r ix e => r -> Comp -> ix -> (ix -> e) -> Array r ix e
makeArrayR _ = makeArray
{-# INLINE makeArrayR #-}


-- | Create a vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> range Seq 1 6
-- (Array D Seq (5)
-- [ 1,2,3,4,5 ])
-- >>> range Seq (-2) 3
-- (Array D Seq (5)
-- [ -2,-1,0,1,2 ])
range :: Comp -> Int -> Int -> Array D Ix1 Int
range comp !from !to = makeArray comp (max 0 (to - from)) (+ from)
{-# INLINE range #-}


-- | Same as `range`, but with a custom step.
--
-- >>> rangeStep Seq 1 2 6
-- (Array D Seq (3)
-- [ 1,3,5 ])
--
rangeStep :: Comp -- ^ Computation strategy
          -> Int -- ^ Start
          -> Int -- ^ Step (Can't be zero)
          -> Int -- ^ End
          -> Array D Ix1 Int
rangeStep comp !from !step !to
  | step == 0 = error "rangeStep: Step can't be zero"
  | otherwise =
    let (sz, r) = (to - from) `divMod` step
    in makeArray comp (sz + signum r) (\i -> from + i * step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toListIx1 $ enumFromN Seq 5 3
-- [5,6,7]
--
enumFromN :: Num e =>
             Comp
          -> e -- ^ Start value
          -> Int -- ^ Length of resulting array
          -> Array D Ix1 e
enumFromN comp !from !sz = makeArray comp sz $ \ i -> fromIntegral i + from
{-# INLINE enumFromN #-}

-- |
--
-- >>> toListIx1 $ enumFromStepN Seq 1 0.1 5
-- [1.0,1.1,1.2,1.3,1.4]
--
enumFromStepN :: Num e =>
                 Comp
              -> e -- ^ Start value
              -> e -- ^ Step value
              -> Int -- ^ Length of resulting vector
              -> Array D Ix1 e
enumFromStepN comp !from !step !sz = makeArray comp sz $ \ i -> from + fromIntegral i * step
{-# INLINE enumFromStepN #-}


