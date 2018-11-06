
```haskell
rangeStepIx :: Index ix => Comp -> ix -> ix -> ix -> Array D ix ix
rangeStepIx comp ixFrom ixStep ixTo =
  makeArray comp sz (\ix -> liftIndex2 (+) ixFrom (liftIndex2 (*) ix ixStep))
  where
    sz = liftIndex2 div (liftIndex2 (-) (liftIndex2 (+) ixStep ixTo) ixFrom) ixStep

rangeStep' :: Index ix => Comp -> ix -> ix -> Array D ix ix
rangeStep' comp ixFrom ixTo =
  makeArray comp sz (\ix -> liftIndex2 (+) ixFrom ix)
  where
    sz = liftIndex2 (-) (liftIndex (+ 1) ixTo) ixFrom


prepMatrix :: (Num ix, Index ix) => ix -> ix -> Array D ix ix
prepMatrix sz s = rangeStep' Par 0 (sz * s - 1)


gaussianMatrix :: (Mutable r Ix2 Double) => r -> Ix2 -> Ix2 -> Double -> Array r Ix2 Double
gaussianMatrix repr sz s@(si :. sj) stdDev = computeAs repr $ fmap gaussian $ rangeStep' Par 0 szs
  where
    gaussian (i :. j) =
      exp
        (-(((fromIntegral i - mds) / sid) ^ (2 :: Int) + ((fromIntegral j - nds) / sjd) ^ (2 :: Int)) /
          var2) /
      (var2 * pi)
    szs@(ms :. ns) = sz * s - 1
    (mds, nds) = (fromIntegral ms / 2, fromIntegral ns / 2)
    (sid, sjd) = (fromIntegral si, fromIntegral sj)
    var2 = 2 * stdDev ^ (2 :: Int)

gaussianIx1 :: Double -> Double -> Double
gaussianIx1 stdDev x = exp (- (x ^ (2 :: Int) / var2)) / sqrt (var2 * pi)
  where
    var2 = 2 * stdDev ^ (2 :: Int)

gaussianIx1' :: Double -> Double
gaussianIx1' = gaussianIx1 1


gaussianIntegral :: (Double, Double) -> Double -> (Double, Double)
gaussianIntegral (pg, acc) x = let curG = gaussianIx1' x in (curG, acc + curG - pg)


gaussianKernel :: Ix2 -> Ix2 -> Array U Ix2 Double
gaussianKernel ksz s = computeWithStrideAs U stride matrix'
  where
    stride = Stride s
    s2Half = liftIndex (`div` 2) s - liftIndex ((`mod` 2) . (+1)) s
    matrix = gaussianMatrix U ksz s 1
    matrix' = extract' s2Half (size matrix - s2Half) $
      mapStencil Edge (integralStencil / fromIntegral (totalElem s)) matrix
    integralStencil = makeConvolutionStencilFromKernel (makeArrayR U Seq s (const 1))


-- integralStencilX :: Double -> Int -> Stencil Ix2 Double Double
-- integralStencilX dx k = makeStencil (1 :. k) 0 $ \g ->
--   snd $ loop 1 (<k) (+1) (g (0 :. 0), 0) $ \ j (jPrev, jAcc) ->
--       let cur = g (0 :. j) in (cur, jAcc + pure dx * (cur - jPrev))


-- integralStencilY :: Double -> Int -> Stencil Ix2 Double Double
-- integralStencilY dx k = makeStencil (k :. 1) 0 $ \g ->
--   loop 0 (<k) (+1) 0 $ \ j jAcc ->
--       let cur = g (0 :. j) in jAcc + pure dx * cur



trapIntegralStencilX :: Double -> Int -> Stencil Ix2 Double Double
trapIntegralStencilX dx k = makeStencil (1 :. k) 0 $ \g ->
  pure dx / 2 * (loop 1 (< k-1) (+1) (g zeroIndex) (\j -> (+ 2 * g (0 :. j))) + g (0 :. k-1))

midpointIntegralStencilX :: Double -> Int -> Stencil Ix2 Double Double
midpointIntegralStencilX dx k =
  makeStencil (1 :. k) 0 $ \g ->
    pure dx * loop 0 (< k) (+1) 0 (\j -> (+ g (0 :. j)))


-- simpson :: Ix1 -> Double -> Double -> Array U Ix2 Double
-- simpson n a b =
--   computeWithStrideAs U (Stride (1 :. nsz)) $
--   mapStencil (Fill 0) (trapezoidStencil 1 dx nsz) v2
--   where
--     d = b - a -- distance
--     dx = d / nFrac
--     nFrac = fromIntegral n
--     nsz = n + 1
--     scale i = a + dx * fromIntegral i
--     v :: Array D Ix1 Double
--     v = fmap scale $ rangeStep' Par 0 n
--     v2 = computeAs U $ resize' (1 :. nsz) $ fmap (\x -> exp (x ^ (2 :: Int))) v


trap :: Ix1 -> Double -> Double -> Array U Ix2 Double
trap n a b =
  computeWithStrideAs U (Stride (1 :. nsz)) $
  mapStencil (Fill 0) (trapezoidStencil 1 dx nsz) v2
  where
    d = b - a -- distance
    dx = d / nFrac
    nFrac = fromIntegral n
    nsz = n + 1
    scale i = a + dx * fromIntegral i
    v :: Array D Ix1 Double
    v = fmap scale $ rangeStep' Par 0 n
    v2 = computeAs U $ resize' (1 :. nsz) $ fmap (\x -> exp (x ^ (2 :: Int))) v


midpoint :: Int -> Double -> Double -> Array U Ix2 Double
midpoint n a b =
  computeWithStrideAs U (Stride (1 :. n)) $
  mapStencil (Fill 0) (midpointIntegralStencilX dx n) v2
  where
    d = b - a
    dx = d / nFrac
    nFrac = fromIntegral n
    dx2 = dx / 2 -- midpoint
    scale i = dx2 + dx * fromIntegral i
    v = fmap scale $ rangeStep' Par 0 (n - 1) :: Array D Ix1 Double
    v2 = computeAs U $ resize' (1 :. n) $ fmap (\x -> exp (x ^ (2 :: Int))) v


-- trap n =
--   computeWithStrideAs U (Stride (1 :. nsz)) $
--   mapStencil (Fill 0) (trapezoidStencil 1 (2 / fromIntegral n) nsz) v2
--   where
--     b = 2
--     nsz = n + 1
--     v = fmap ((/ (fromIntegral n / b)) . fromIntegral) $ rangeStep' Par 0 n :: Array D Ix1 Double
--     v2 = computeAs U $ resize' (1 :. nsz) $ fmap (\x -> exp (x ^ (2 :: Int))) v





lowerDim ::
     (Index (Lower ix), Size r ix e) => Dim -> Array r ix e -> Maybe (Array r (Lower ix) e)
lowerDim dim arr = do
  let sz = size arr
  1 <- getIndex sz dim
  newSz <- dropDim sz dim
  resize newSz arr


lowerDim' :: (Size r ix e, Index (Lower ix)) => Dim -> Array r ix e -> Array r (Lower ix) e
lowerDim' dim arr =
  case lowerDim dim arr of
    Just ix' -> ix'
    Nothing ->
      error $ "lowerDim': Dimension is out of reach or it's value isn't equal to 1: " ++ show dim

raiseDim :: (Size r (Lower ix) e, Index ix) => Array r (Lower ix) e -> Array r ix e
raiseDim arr = unsafeResize (consDim 1 (size arr)) arr

raiseDim' :: (Size r (Lower ix) e, Index ix) => Array r (Lower ix) e -> Array r ix e
raiseDim' arr = unsafeResize (snocDim (size arr) 1) arr


midpointStencil ::
     (Fractional e, Index ix)
  => Dim -- ^ Dimension along which to integrate
  -> e -- ^ Δx - distance between sample points
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
midpointStencil dim dx k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim k) zeroIndex $ \g ->
    pure dx * loop 0 (< k) (+ 1) 0 (\i -> (+ g (setIndex' zeroIndex dim i)))

trapezoidStencil ::
     (Fractional e, Index ix)
  => Dim -- ^ Dimension along which to integrate
  -> e -- ^ Δx - distance between sample points
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
trapezoidStencil dim dx k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim k) zeroIndex $ \g ->
    pure dx / 2 *
    (loop 1 (< k - 1) (+ 1) (g zeroIndex) (\i -> (+ 2 * g (setIndex' zeroIndex dim i))) +
     g (setIndex' zeroIndex dim (k - 1)))

simpsonStencil ::
     (Fractional e, Index ix)
  => Dim -- ^ Dimension along which to integrate
  -> e -- ^ Δx - distance between sample points
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
simpsonStencil dim dx k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim k) zeroIndex $ \g ->
    let simAcc i (prev, acc) =
          let !fx3 = g (setIndex' zeroIndex dim (i + 2))
              !newAcc = acc + prev + 4 * g (setIndex' zeroIndex dim (i + 1)) + fx3
           in (fx3, newAcc)
     in pure dx / 3 * snd (loop 2 (< k - 2) (+ 2) (simAcc 0 (g zeroIndex, 0)) simAcc)

-- TODO: implement `overIndex :: ix -> Dim -> (Int -> Int) -> Maybe ix`

-- http://tutorial.math.lamar.edu/Classes/CalcII/ApproximatingDefIntegrals.aspx

integrateWith ::
     (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
  => (Dim -> e -> Stencil ix e e)
  -> Dim
  -> Int
  -> e
  -> Array r ix e
  -> Array r' ix e
integrateWith stencil dim n d arr =
  computeWithStride (Stride nsz) $
  mapStencil (Fill 0) (stencil dim dx) arr
  where
    nFrac = fromIntegral n
    dx = d / nFrac
    nsz = setIndex' (pureIndex 1) dim n

simpsonsRule ::
     (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
  => r'
  -> Dim
  -> Int
  -> e
  -> Array r ix e
  -> Array r' ix e
simpsonsRule _ dim n = integrateWith (\dim' dx -> simpsonStencil dim' dx (n + 1)) dim n


trapezoidRule ::
     (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
  => r'
  -> Dim
  -> Int
  -> e
  -> Array r ix e
  -> Array r' ix e
trapezoidRule _ dim n = integrateWith (\dim' dx -> trapezoidStencil dim' dx (n + 1)) dim n


midpointRule ::
     (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
  => r'
  -> Dim
  -> Int
  -> e
  -> Array r ix e
  -> Array r' ix e
midpointRule _ dim n = integrateWith (\dim' dx -> midpointStencil dim' dx n) dim n


integralApproxIx2 ::
     (Fractional e, Mutable r Ix2 e)
  => r -- ^ Which representation to use for intermediate computed arrays.
  -> Int -- ^ Samples per interval
  -> Ix2 -- ^ Result size of the matrix
  -> (e -> e -> e) -- ^ f(x, y) - function to integrate
  -> e -- ^ Integrate from @a@
  -> e -- ^ Length of interval @d@
  -> Array M Ix2 e
integralApproxIx2 r n sz f a d =
  extract' 0 sz $
  toManifest $
  simpsonsRule r 2 n dx $
  simpsonsRule r 1 n dx $ computeAs r arr
  where
    nFrac = fromIntegral n
    dx = d / nFrac
    arr =
      fmap (\(i :. j) -> f (a + d * fromIntegral i / nFrac) (a + d * fromIntegral j / nFrac)) $
      rangeStep' Par zeroIndex (pureIndex n * sz)


integralApproxIx2' ::
     (Fractional e, Load s Ix2 e, Mutable r Ix2 e)
  => r -- ^ Which representation to use for intermediate computed arrays.
  -> Ix2 -- ^ Result size of the matrix
  -> e -- ^ Length of interval @d@
  -> Array s Ix2 e
  -> Array M Ix2 e
integralApproxIx2' r sz d arr =
  extract' 0 sz $
  toManifest $
  --simpsonsRule r 2 n dx $
  simpsonsRule r 1 (getIndex' n 1) dx $ computeAs r arr
  where
    n = liftIndex2 div (size arr) sz
    nFrac = fromIntegral $ getIndex' n 1
    dx = d / nFrac
    -- arr =
    --   fmap (\(i :. j) -> f (a + d * fromIntegral i / nFrac) (a + d * fromIntegral j / nFrac)) $
    --   rangeStep' Par zeroIndex (pureIndex n * sz)


integrate ::
     (Mutable r Ix2 Double) => Dim -> Int -> Double -> Array r Ix2 Double -> Array r Ix2 Double
integrate dim n d arr =
  computeWithStride (Stride nsz) $
  extract' 0 (setIndex' sz dim (k - (k `mod` n))) $
  mapStencil (Fill 0) (simpsonStencil dim (d / nFrac) n') arr
  where
    k = getIndex' sz dim
    sz = size arr
    nFrac = fromIntegral n
    n' = n + 1
    nsz = setIndex' (pureIndex 1) dim n

gauss :: Dim -> Ix1 -> Double -> Double -> Array U Ix2 Double
gauss dim n a b = integrate dim n d v2
  where
    d = b - a
    nFrac = fromIntegral n
    nsz = setIndex' (pureIndex 1) dim (n * 2)
    v :: Array D Ix1 Double
    v = fmap (\i -> a + d * fromIntegral i / nFrac) $ rangeStep' Par 0 (n * 2 - 1)
    v2 = computeAs U $ resize' nsz $ fmap (\x -> exp (x ^ (2 :: Int))) v

integrateIx2 ::
     (Mutable r Ix2 Double)
  => r
  -> Int -- ^ Samples per interval
  -> Ix2 -- ^ Result size of the matrix
  -> (Double -> Double -> Double) -- ^ f(x, y) - function to integrate
  -> Double -- ^ Integrate from @a@
  -> Double -- ^ Length of interval @d@
  -> Array r Ix2 Double
integrateIx2 _ n sz f a d = integrate 2 n d $ integrate 1 n d $ compute arr
  where
    nFrac = fromIntegral n
    arr =
      fmap (\(i :. j) -> f (a + d * fromIntegral i / nFrac) (a + d * fromIntegral j / nFrac)) $
      rangeStep' Par zeroIndex (pureIndex n * sz)

ga _ x = exp (x ^ (2 :: Int))

gaussianIx2 :: Floating a => a -> a -> a -> a
gaussianIx2 stdDev y x = exp (-(x ^ (2 :: Int) + y ^ (2 :: Int)) / var2) / (var2 * pi)
  where
    var2 = 2 * stdDev ^ (2 :: Int)

-- integrate dim n a b =
--   computeWithStrideAs U (Stride nsz) $
--   mapStencil (Fill 0) (trapezoidStencil dim (d / nFrac) nsz) v2
--   where
--     d = b - a -- distance
--     nFrac = fromIntegral n
--     n' = n + 1
--     nsz = setIndex' (pureIndex 1) dim n'
--     v :: Array D Ix1 Double
--     v = fmap (\ i -> a + d * fromIntegral i / nFrac) $ rangeStep' Par 0 n
--     v2 = computeAs U $ resize' nsz $ fmap (\x -> exp (x ^ (2 :: Int))) v


-- trapezoidStencil (\(i :> j :. k) p -> (i :> j :> p :. k)
```


Later improvements

```haskell

integralApprox1 ::
     (Fractional e, Mutable r Ix1 e)
  => (e -> Dim -> Int -> Stencil Ix1 e e) -- ^ Integration Stencil
  -> Ix1 -- ^ Result size of the matrix
  -> e -- ^ Length of interval @d@
  -> Array r Ix1 e
  -> Array M Ix1 e
integralApprox1 stencil sz d arr = extract' 0 sz $ toManifest $ applyStencil 1 (getIndex' n 1) arr
  where
    applyStencil = integrateWith (stencil dx)
    {-# INLINE applyStencil #-}
    n = liftIndex2 div (size arr) sz
    nFrac = fromIntegral $ getIndex' n 1
    dx = d / nFrac
{-# INLINE integralApprox1 #-}

integralApprox2 ::
     (Fractional e, Mutable r Ix2 e)
  => (e -> Dim -> Int -> Stencil Ix2 e e) -- ^ Integration Stencil
  -> Ix2 -- ^ Result size of the matrix
  -> Int
  -> e -- ^ Length of interval @d@
  -> Array r Ix2 e
  -> Array M Ix2 e
integralApprox2 stencil sz n d arr =
  extract' 0 sz $ toManifest $ applyStencil 2 n $ applyStencil 1 n arr
  where
    applyStencil = integrateWith (stencil dx)
    {-# INLINE applyStencil #-}
    nFrac = fromIntegral $ getIndex' n 1
    dx = d / nFrac
{-# INLINE integralApprox2 #-}



fromFunctionMidpoint2
  :: Fractional a =>
     (a -> a -> e) -> a -> a -> Ix2 -> Int -> Array D Ix2 e
fromFunctionMidpoint2 f a d sz n = fromFunction2 f (a + dx2) d sz n
  where
    nFrac = fromIntegral n
    dx2 = d / nFrac / 2




-- trapezoidRule ::
--      (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
--   => r'
--   -> Dim
--   -> Int
--   -> e
--   -> Array r ix e
--   -> Array r' ix e
-- trapezoidRule _ dim n dx = integrateWith (trapezoidStencil dim (n + 1) dx) dim n


-- simpsonsRule ::
--      (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
--   => r'
--   -> Dim
--   -> Int
--   -> e
--   -> Array r ix e
--   -> Array r' ix e
-- simpsonsRule _ dim n dx = integrateWith (simpsonStencil dim (n + 1) dx) dim n




fromFunction1
  :: Fractional a =>
     Comp -> (a -> b) -> a -> a -> Ix1 -> Int -> Array D Ix1 b
fromFunction1 comp f a d sz n =
  fmap (\i -> f (scale i)) $ range' comp 0 (n * sz)
  where
    nFrac = fromIntegral n
    scale i = a + d * fromIntegral i / nFrac
    {-# INLINE scale #-}
{-# INLINE fromFunction1 #-}


fromFunctionMidpoint
  :: (Integral a, Fractional t1) =>
     (t2 -> t3 -> t1 -> t1 -> t4 -> a -> t5)
     -> t2 -> t3 -> t1 -> t1 -> t4 -> a -> t5
fromFunctionMidpoint fromFunction comp f a d sz n = fromFunction comp f (a + dx2) d sz n
  where
    nFrac = fromIntegral n
    dx2 = d / nFrac / 2
{-# INLINE fromFunctionMidpoint #-}


fromFunctionMidpoint1
  :: Fractional a =>
     (a -> b) -> a -> a -> Ix1 -> Int -> Array D Ix1 b
fromFunctionMidpoint1 f a d sz n = fromFunction1 f (a + dx2) d sz n
  where
    nFrac = fromIntegral n
    dx2 = d / nFrac / 2

```
