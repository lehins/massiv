{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Prim.Internal where







loopM_ :: Monad m => t -> (t -> t) -> (t -> Bool) -> (t -> m a) -> m ()
loopM_ !init next cond f = go init where
  go !step =
    case cond step of
      False -> return ()
      True  -> f step >> go (next step)
  {-# INLINE go #-}
{-# INLINE loopM_ #-}


