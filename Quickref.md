# Massiv Quick Reference

## Indices

Everyone is well accustomed to the fact that the order of indices corresponds to the number of
dimensions an array can have in the reverse order, eg in `C`: `arr[i][j][k]` will mean that a
3-dimensional array is indexed at an outermost 3rd dimension with index `i`, 2nd dimension `j` and
the innermost 1st dimension `k`. In the case of a 3D world `i` points to a page, `j` to a row and
`k` to a column, but the astraction scales naturally to any dimension as long as we agree on the
order of things. Below are various ways to index an array in massiv:

```haskell
λ> arr = makeArrayR U Seq (Sz (2 :> 3 :. 4)) $ \ (i :> j :. k) -> i + j ^ k
λ> arr ! 1 :> 2 :. 3
9
λ> arr !> 1 !> 2
(Array M Seq (Sz1 (4))
  [ 2,3,5,9 ])
λ> arr !> 1 !> 2 ! 3
9
```

Former does the lookup of an element in the array, while the latter slices the array until it gets
to a row and only then looks up the actual element.

Data is represented in a linear row-major fashion, so the above indexing technique translates into a
linear index that will get mapped into an element in memory at some point.


## Hierarchy

### Class dependency

```
Size (D, DL, DI, B, BN, BL, P, U, S)
Shape (D, DL, DS, DI, DW, B, BN, BL, P, U, S, L, LN)
StrideLoad (DI, DW) -> Load (DL, DS, L) -> Source (D) -> Manifest (B, BN, BL, P, U, S)
                                       \
                                        `-> Stream (D, B, BN, BL, P, U, S, L)
```

## Computation

As you know arrays can be computed in parallel or sequentially in `massiv`, but there is a lot more
to that:

* Normally you can supply computation strategy as an argument (`Seq`/`Par`) during the array
  construction or conversion, eg. from a list or vector
* array computation strategy will be combined according to its `Monoid` instance when two or more
  arrays are being joined together by some operation into another one.
* Most functions will respect the inner computation strategy, while others will ignore it due to
  their specific nature.

## Naming Conventions

### Levels of Safety

There are three levels of safety that are commonly used in `massiv` API
functions:

* unsafeFunctionName
* functionName'
* functionName

Functions with the prefix `unsafe` don't do bounds checking. You should only use
these in situations where you can prove that the bounds will be correct. The
prime suffix is used in cases where the non-primed version of the function could
fail for other reasons and is therefore more cumbersome to use (it returns a
`Maybe`, needs type-level dimensions, et0). The primed version calls `error`
instead of returning `Maybe` and should only be used when you know that a
`Nothing` will not be returned.

Using the primed version gives you convenience. Using the `unsafe` version gives
you performance.

### Dimension Variations

There are three common variations of functions dealing with dimensions:

* fooWithin
* fooOuter
* fooInner

The `Within` suffix signals that the function operates on a particular dimension
and it allows you to pick any dimension you want. Functions with the `Outer`
suffix use dimension `n` in an n-dimensional index don't take a dimension
argument. Functions with the `Inner` suffix use dimension `1`.


## Examples

### Conversion from `array`

Here is an example of how to convert a nested boxed array from `array` package to a
rectangular `Matrix` with parallelization:


```haskell
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Control.Scheduler (scheduleWork_)
import qualified Data.Array.IArray as IA

toPrimArray :: IA.Array Int (IA.Array Int Float) -> IO (Array P Ix2 Float)
toPrimArray arr = do
  let m = IA.rangeSize (IA.bounds arr)
      n | m > 0 = IA.rangeSize (IA.bounds (arr IA.! 0))
        | otherwise = 0
  createArray_ Par (Sz2 m n) $ \scheduler marr ->
    A.forM_ (0 ..: m) $ \i -> do
      let vec = arr IA.! i
      scheduleWork_ scheduler $ A.forM_ (0 ..: n) $ \j ->
        unsafeWrite marr (i :. j) (vec IA.! j)
```

Sample run:

```haskell
squareRaggedArray :: IA.Array Int (IA.Array Int Float)
squareRaggedArray =
  IA.listArray (0, m - 1)
  [ IA.listArray (0, n - 1) [fromIntegral (x + y) | x <- [0 .. n]] | y <- [0 .. m]]
  where
    (m, n) = (3, 4)
```

```haskell
λ> squareRaggedArray
array (0,2) [(0,array (0,3) [(0,0.0),(1,1.0),(2,2.0),(3,3.0)]),(1,array (0,3) [(0,1.0),(1,2.0),(2,3.0),(3,4.0)]),(2,array (0,3) [(0,2.0),(1,3.0),(2,4.0),(3,5.0)])]
λ> toPrimArray squareRaggedArray
Array P Par (Sz (3 :. 4))
  [ [ 0.0, 1.0, 2.0, 3.0 ]
  , [ 1.0, 2.0, 3.0, 4.0 ]
  , [ 2.0, 3.0, 4.0, 5.0 ]
  ]
```
