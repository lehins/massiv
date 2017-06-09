# Stencil with SIMD


Horizontal Sobel filter for Doubles implemented with 128bit SIMD
instructions. It is marginally faster (~10%) than regular Sobel Stencil, thus it
is not worth the trouble:
* hard dependency on llvm backend,
* type restriction of a filter and
* reliance on CPP preprocessing to accomodate different vector width supported
  by a system.

At the same time, it might be a very good approach for increasing performance in case of large
convolution filters that operate on low precision values (Word8, Word16, etc.). 

```haskell
sobelXSIMD :: Border Double -> Stencil M.DIM2 Double Double
sobelXSIMD b = mkStaticStencil b (3, 3) (1, 1) stencil where
  stencil f = D# (j0 +## j1) where
    !(D# i0) = f (-1, -1)
    !(D# i1) = f ( 0, -1)
    !(D# i2) = f ( 1, -1)
    !(D# i3) = f (-1,  1)
    !(D# i4) = f ( 0,  1)
    !(D# i5) = f ( 1,  1)
    !i02 = packDoubleX2# (# i0, i2 #)
    !i14 = packDoubleX2# (# i1, i4 #)
    !i35 = packDoubleX2# (# i3, i5 #)
    !i0124 = plusDoubleX2# i02 (timesDoubleX2# i14 (packDoubleX2# (# 2.0##, -2.0## #)))
    !(# j0, j1 #) =
      unpackDoubleX2# (plusDoubleX2# i0124 (negateDoubleX2# i35))
  {-# INLINE stencil #-}
{-# INLINE sobelXSIMD #-}
```


```bash
$ stack bench massiv:convolution --benchmark-arguments="--match prefix \"Sobel Horizontal\""
massiv-0.1.0.0: benchmarks
Running 1 benchmarks...
Benchmark convolution: RUNNING...
benchmarking Sobel Horizontal/Massiv mapStencil
time                 3.648 ms   (3.642 ms .. 3.652 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.653 ms   (3.650 ms .. 3.656 ms)
std dev              10.56 μs   (8.844 μs .. 13.10 μs)

benchmarking Sobel Horizontal/Massiv mapStencil SIMD
time                 3.170 ms   (3.162 ms .. 3.176 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.170 ms   (3.166 ms .. 3.175 ms)
std dev              14.90 μs   (11.44 μs .. 19.64 μs)

benchmarking Sobel Horizontal/VectorConvolve
time                 3.529 ms   (3.457 ms .. 3.609 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 3.463 ms   (3.447 ms .. 3.494 ms)
std dev              68.88 μs   (37.82 μs .. 119.1 μs)

benchmarking Sobel Horizontal/Repa Sobel
time                 9.172 ms   (8.988 ms .. 9.290 ms)
                     0.993 R²   (0.978 R² .. 1.000 R²)
mean                 9.116 ms   (8.955 ms .. 9.654 ms)
std dev              719.5 μs   (203.3 μs .. 1.553 ms)
variance introduced by outliers: 44% (moderately inflated)
```
