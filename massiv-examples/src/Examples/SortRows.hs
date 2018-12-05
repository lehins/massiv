{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Examples.SortRows where

import           Data.Massiv.Array                 as A
import           Data.Massiv.Array.Manifest.Vector as A
import           Data.Massiv.Core.Scheduler
import           Data.Typeable
import           Data.Vector.Algorithms.Merge
import           Data.Vector.Generic               as VG
import           Data.Vector.Generic.Mutable       as VGM
import           System.IO.Unsafe

sortRows ::
     forall r e v.
     (Ord e, Typeable v, A.Mutable r Ix2 e, VG.Vector v e, ARepr v ~ r, VRepr r ~ v)
  => Array r Ix2 e
  -> Array r Ix2 e
sortRows arr = unsafePerformIO $ do
  mv :: VG.Mutable v RealWorld e <- VG.thaw (A.toVector arr :: v e)
  let comp = getComp arr
      sz@(m :. n) = size arr
  case comp of
    Seq -> do
      loopM_ 0 (< m) (+ 1) $ \i -> sort $ VGM.slice (toLinearIndex sz (i :. 0)) n mv
    ParOn wIds ->
      withScheduler_ wIds $ \scheduler -> do
        loopM_ 0 (< m) (+ 1) $ \i ->
          scheduleWork scheduler $ sort $ VGM.slice (toLinearIndex sz (i :. 0)) n mv
  v :: v e <- VG.unsafeFreeze mv
  return $ A.fromVector comp sz v
