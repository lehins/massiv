# massiv

`massiv` is a Haskell library for array manipulation. Performance is one of its
main goals, thus it is able to run effortlessly almost all operations in
parallel as well as sequentially.

The name for this library comes from the Russian word Massiv (Масси́в), which
means an Array.

## Status

__Disclaimer__: The current status of this library is still under development, but it is already at
a rather stable point, so no significant API changes should happen.

| Language | `massiv` | `massiv-io` | Travis | AppVeyor
|:---:|:---:|:---:|:---:|:---:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/massiv.svg) | [![Hackage](https://img.shields.io/hackage/v/massiv.svg)](https://hackage.haskell.org/package/massiv) | [![Hackage](https://img.shields.io/hackage/v/massiv-io.svg)](https://hackage.haskell.org/package/massiv-io) | [![Travis](https://img.shields.io/travis/lehins/massiv/master.svg)](https://travis-ci.org/lehins/massiv) | [![AppVeyor](https://img.shields.io/appveyor/ci/lehins/massiv.svg)](https://ci.appveyor.com/project/lehins/massiv)

## Introduction

Everything in the library revolves around an `Array r ix e` - a data type
family for anything that can be thought of as an array. The type variables,
from the end, are:

* `e` - element of an array.
* `ix` - an index that will map to an actual element. The index must be an instance of the `Index` class
  with the default one being an `Ix n` type family and an optional being tuples of `Int`s.
* `r` - underlying representation. The main representations are:

    * `D` - delayed array, which is simply a function from an index to an element: `(ix ->
      e)`. Therefore indexing of this type of array is not possible, although elements can be
      computed with the `evaluateAt` function.
    * `P` - Array with elements that are an instance of `Prim` type class, i.e. common Haskell
      primitive types: `Int`, `Word`, `Char`, etc. Backed by the usual `ByteArray`.
    * `U` - Unboxed arrays. The elements are instances of the `Unbox` type class. Just as fast as
      `P`, but has a wider range of data types that it can work with. Notable data types that can be
      stored as elements are `Bool`, tuples and `Ix n`.
    * `S` - Storable arrays. Backed by a pinned `ByteArray`s and elements are instances of the
      `Storable` type class.
    * `B` - Boxed arrays that don't have restrictions on their elements, since they are represented
      as pointers to elements, thus making them the slowest type of array, but also the most
      general. Arrays of this representation are element strict, in other words its elements are
      kept in Weak-Head Normal Form (WHNF).
    * `N` - Also boxed arrays, but unlike the other representation `B`, its elements are in Normal
      Form, i.e. in a fully evaluated state and no thunks or memory leaks are possible. It does
      require `NFData` instance for the elements though.
    * `M` - Manifest arrays, which is a general type of array that is backed by some memory
      representation, therefore any of the above `P`, `U`, `S`, `B` type of arrays can be converted
      to `M` in constant time with `toManifest` function. It is mostly useful during constant time
      slicing of manifest arrays, as this becomes the result representation. More on that in the
      [slicing](#slicing) section.

## Construct

Creating a delayed type of array allows us to fuse any future operation we decide
to perform on it. Let's look at this example:

```haskell
λ> import Data.Massiv.Array as A
λ> let vec = makeVectorR D Seq 10 id
λ> vec
(Array D Seq (10)
  [ 0,1,2,3,4,5,6,7,8,9 ])
```

Here we created a delayed vector of size 10, which is in reality just an `id`
function from its index to an element (see the [Computation](#computation)
section for the meaning of `Seq`). So let's go ahead and square its elements

```haskell
λ> evaluateAt vec 4
4
λ> let vec2 = fmap (^ (2::Int)) vec
λ> evaluateAt vec2 4
16
```

It's not that exciting, since every time we call `evaluateAt` it will recompute
the element, __every time__, therefore this function should be avoided at all
costs. Instead we can use all of the functions that take `Source` like arrays
and then fuse that computation together by calling `compute`, or a handy
`computeAs` function and only afterwards apply an `index'` function or its
synonym: `(!)`. Any delayed array can also be reduced using one of the folding
functions, thus completely avoiding any memory allocation, or converted to a
list, if that's what you need:

```haskell
λ> let vec2U = computeAs U vec2
λ> vec2U
(Array U Seq (10)
  [ 0,1,4,9,16,25,36,49,64,81 ])
λ> vec2U ! 4
16
λ> toList vec2U
[0,1,4,9,16,25,36,49,64,81]
λ> A.sum vec2U
285
```

Other means of constructing arrays are through conversion from lists, vectors from the `vector`
library and using a few other helper functions as `range`, `enumFromN`, etc. It's worth noting that, in the
next example, nested lists will be loaded into an unboxed manifest array and the sum of its elements
will be computed in parallel on all available cores.

```haskell
λ> A.sum (fromLists' Par [[0,0,0,0,0],[0,1,2,3,4],[0,2,4,6,8]] :: Array U Ix2 Double)
30.0
```

The above wouldn't run in parallel in ghci of course, as the program would have
to be compiled with ghc and `-threaded -with-rtsopts=-N` flags in order to use
all available cores. Alternatively we could do compile with the `-threaded`
flag and then pass the number of capabilities directly to the runtime with
`+RTS -N<n>`, where `<n>` is the number of cores you'd like to utilize.

## Index

The main `Ix n` closed type family can be somewhat confusing, but there is no
need to fully understand how it is implemented in order to start using it. GHC
might ask you for the `DataKinds` language extension if `IxN n` is used in a
type signature.

There are three distinguishable constructors for the index:

* The first one is simply an int: `Ix1 = Ix 1 = Int`, therefore vectors can be indexed in a usual way
  without some extra wrapping data type, just as it was demonstrated in a previous section.
* The second one is `Ix2` for operating on 2-dimensional arrays and has a constructor `:.`

```haskell
λ> makeArrayR D Seq (3 :. 5) (\ (i :. j) -> i * j)
(Array D Seq (3 :. 5)
  [ [ 0,0,0,0,0 ]
  , [ 0,1,2,3,4 ]
  , [ 0,2,4,6,8 ]
  ])
```

* The third one is `IxN n` and is for working with N-dimensional arrays, and has a similar looking
  constructor `:>`, except that it can be chained indefinitely on top of `:.`

```haskell
λ> :t makeArrayR D Seq (10 :> 20 :. 30) $ \ (i :> j :. k) -> i * j + k
makeArrayR D Seq (10 :> 20 :. 30) $ \ (i :> j :. k) -> i * j + k
  :: Array D (IxN 3) Int
λ> :t (10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1) -- 10-dimensional index
(10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1) -- 10-dimensional index
  :: IxN 10
```

Here is how to construct a 4-dimensional array and sum its elements in constant
memory:

```haskell
λ> let arr = makeArrayR D Seq (10 :> 20 :> 30 :. 40) $ \ (i :> j :> k :. l) -> (i * j + k) * k + l
λ> :t arr -- a 4-dimensional array
arr :: Array D (IxN 4) Int
λ> A.sum arr
221890000
```

Alternatively tuples of `Int`s can be used for working with Arrays, up to and
including 5-tuples (type synonyms: `Ix2T` - `Ix5T`), but since tuples are
polymorphic it is necessary to restrict the resulting array type:

```haskell
λ> makeArray Seq (4, 20) (uncurry (*)) :: Array P Ix2T Int
(Array P Seq ((4,20))
  [ [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ]
  , [ 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19 ]
  , [ 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38 ]
  , [ 0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57 ]
  ])
λ> :i Ix2T
type Ix2T = (Int, Int)
```

There are helper functions that can go back and forth between indices. Also `Ix n` is an instance of `Num` so basic numeric operations are made easier:

```haskell
λ> fromIx4 (3 :> 4 :> 5 :. 6)
(3,4,5,6)
λ> toIx5 (3,4,5,6,7)
3 :> 4 :> 5 :> 6 :. 7
λ> (1 :> 2 :. 3) + (3 :> 2 :. 1)
4 :> 4 :. 4
```

## Slicing

In order to get a subsection of an array there is no need to recompute it,
unless we want to free up the extra memory, of course. So, there are a few
slicing, resizing and extraction operators that can do it all in constant time,
modulo the index manipulation:

```haskell
λ> let arr = makeArrayR U Seq (4 :> 2 :. 6) fromIx3
λ> arr !> 3 !> 1
(Array M Seq (6)
  [ (3,1,0),(3,1,1),(3,1,2),(3,1,3),(3,1,4),(3,1,5) ])
```

As you might suspect all of the slicing, indexing, extracting, resizing operations are partial,
and those are frowned upon in Haskell. So there are matching functions that can do the same
operations safely by returning `Nothing` on failure.

```haskell
λ> arr !?> 3 ??> 1
Just (Array M Seq (6)
  [ (3,1,0),(3,1,1),(3,1,2),(3,1,3),(3,1,4),(3,1,5) ])
λ> arr !?> 3 ??> 1 ??> 0
Just (3,1,0)
```


In above examples we first take a slice at the 3rd page, then another one at the 1st row (both counts start
at 0). While in the last example we also take 0th element. Pretty neat, huh? Naturally, by doing a
slice we always reduce dimension by one. We can do slicing from the outside as well as from the
inside:

```haskell
λ> let a = resize' (3 :. 3) $ range Seq 1 10
λ> a
(Array D Seq (3 :. 3)
  [ [ 1,2,3 ]
  , [ 4,5,6 ]
  , [ 7,8,9 ]
  ])
λ> a !> 0
(Array D Seq (3)
  [ 1,2,3 ])
λ> a <! 0
(Array D Seq (3)
  [ 1,4,7 ])
```

Or we can slice along any dimension:

```haskell
λ> a <!> (2, 0)
(Array D Seq (3)
  [ 1,2,3 ])
λ> a <!> (1, 0)
(Array D Seq (3)
  [ 1,4,7 ])
```

In order to extract sub-array while preserving dimensionality we can use `extract` or `extractFromTo`.

```haskell
λ> extract' 0 (1 :. 3) a
(Array D Seq (1 :. 3)
  [ [ 1,2,3 ]
  ])
λ> extract' 0 (3 :. 1) a
(Array D Seq (3 :. 1)
  [ [ 1 ]
  , [ 4 ]
  , [ 7 ]
  ])
```

## Computation

There is a data type `Comp` that controls how elements will be computed when calling the `compute`
function. It has two constructors:

* `Seq` - computation will be done sequentially on one core.
* `ParOn [Int]` - perform computation in parallel while pinning the workers to particular
  cores. Providing an empty list will result in the computation being distributed over all available
  cores, or better known in Haskell as capabilities.
* `Par` - isn't really a constructor but a `pattern` for constructing `ParOn []`, thus should be
  used instead of `ParOn`.

Just to make sure a simple novice mistake is prevented, which I have seen in the past, make sure
your source code is compiled with `ghc -O2 -threaded -with-rtsopts=-N`, otherwise no parallelization
and poor performance are waiting for you. Also a bit later you might notice the `{-# INLINE funcName
#-}` pragma being used, often times it is a good idea to do that, but not always required. It is
worthwhile to benchmark and experiment.

## Stencil

Instead of manually iterating over a multidimensional array and applying a function to each element,
while reading its neighboring elements (as you would do in an imperative language) in a functional
language it is much more efficient to apply a stencil function and let the library take care of all
of bounds checking and iterating in a cache friendly manner.

What's a [stencil](https://en.wikipedia.org/wiki/Stencil_code)? It is a
declarative way of specifying a pattern for how elements of an array in a
neighborhood will be used in order to update each element of that array. In
massiv a stencil is a function that can read the neighboring elements of the
stencil's _center_ (the zero index), and only those, and then outputs a new
value for the center element.

![stencil](massiv-examples/files/stencil.png)

Let's create a simple, but somewhat meaningful array and create an averaging
stencil. There is nothing particular about the array itself, but the filter is
a stencil that sums the elements in a [Moore
neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) and
divides the result by 9, i.e. finds the average of a 3 by 3 square.

```haskell
arrLightIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrLightIx2 comp arrSz = makeArray comp arrSz lightFunc
    where lightFunc (i :. j) = sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}

average3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3Filter b = makeStencil b (3 :. 3) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}
```

Here is what it would look like in GHCi. We create a delayed array with some
funky periodic function, and make sure it is computed prior to mapping an
average stencil over it:

```haskell
λ> let arr = computeAs U $ arrLightIx2 Par (600 :. 800)
λ> :t arr
arr :: Array U Ix2 Double
λ> :t mapStencil (average3x3Filter Edge) arr
mapStencil (average3x3Filter Edge) arr :: Array DW Ix2 Double
```

As you can see, that operation produced an array of some weird representation `DW`, which stands for
Delayed Windowed array. In its essence `DW` is an array type that does no bounds checking in order
to gain performance, except when it's near the border, where it uses a border resolution technique
supplied by the user (`Edge` in the example above). Currently it is used only in stencils and
not much else can be done to an array of this type besides further computing it into a
manifest representation.

This example will be continued in the next section, but before that I would like to mention that
some might notice that it looks very much like convolution, and in fact convolution can be
implemented with a stencil. There is a helper function `mkConvolutionStencil` that lets
you do just that. For the sake of example we'll do a sum of all neighbors by hand instead:

```haskell
sum3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
sum3x3Filter b = mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE sum3x3Filter #-}
```

There is not a single plus sign, that is because convolutions is actually summation of elements, so
instead we have composition of functions applied to an offset index and a multiplier. After we map
that stencil, we can further divide each element of the array by 9 in order to get the
average. Yeah, I lied a bit, `Array DW ix` is an instance of `Functor` class, so we can map functions
over it, which will be fused as with a regular `D`elayed array:

```haskell
computeAs U $ fmap (/9) . mapStencil (sum3x3Filter Edge) arr
```

# massiv-io

In order to do anything useful with arrays we need to be able to read some data
from a file. Considering that most common array-like files are images,
[massiv-io](massiv-io) provides an interface to read, write and display images in common
formats using Haskell native JuicyPixels and Netpbm packages.

There is also a variety of colorspaces (or rather color models) and pixel types
that are currently included in this package, which will likely find a separate
home in the future, but for now we can ignore those colorspaces and
pretend that `Pixel` is some magic wrapper around numeric values that this
package is capable of reading/writing.

The previous example wasn't particularly interesting, since we couldn't visualize
what is actually going on, so let's expend on it:

```haskell
import Data.Massiv.Array.IO

main :: IO ()
main = do
  let arr = arrLightIx2 Par (600 :. 800)
      img = computeAs S $ fmap PixelY arr -- convert an array into a grayscale image
  writeImage "files/light.png" img
  writeImage "files/light_avg.png" $ computeAs S $ mapStencil (average3x3Filter Edge) img
```

`massiv-examples/files/light.png`:

![Light](massiv-examples/files/light.png)

`massiv-examples/files/light_avg.png`:

![Light](massiv-examples/files/light_avg.png)


The full example is in the [massiv-examples](massiv-examples) package and if you have stack installed
you can run it as:

```bash
$ cd massiv-examples/ && stack build && stack exec -- examples
```

# Other libraries

A natural question might come to mind: Why even bother with a new array library when we already have
a few really good ones in the Haskell world? The main reasons for me are performance and usability. I
personally felt like there was much room for improvement even before I started work on this package,
and it seems as it turned out to be true. For example, the most common goto library for dealing with
multidimensional arrays and parallel computation is [Repa](https://hackage.haskell.org/package/repa), which I personally was a big fan of for
the longest time, to the point that I even wrote a [Haskell Image
Processing](https://hackage.haskell.org/package/hip) library based on it.

Here is a quick summary of how `massiv` compares to Repa so far:

* Better scheduler, that is capable of handling nested parallel computation.
* Still shape polymorphic, but with improved default indexing data types.
* Safe stencils for arbitrary dimensions, not only convolution. Stencils are composable through an
  instance of Applicative
* Improved performance on almost all operations. (I might be wrong here, but so far it looks
  promising and rigorous benchmarks are coming to prove the claim)
* Structural parallel folds (i.e. left/right - direction is preserved)
* Super easy slicing.
* Delayed arrays aren't indexable, only Manifest are (saving user from common pitfall in Repa of
  trying to read elements of delayed array)

As far as usability of the library goes, it is very subjective, thus I'll let you be a judge of
that. When talking about performance it is the facts that do matter. Thus, let's not continue this
discussion in pure abstract words, below is a glimpse into benchmarks against Repa library running
with GHC 8.2.2 on Intel® Core™ i7-3740QM CPU @ 2.70GHz × 8

Stencil example discussed earlier:

```
Benchmark convolve-seq: RUNNING...
benchmarking Stencil/Average/Massiv Parallel
time                 6.859 ms   (6.694 ms .. 7.142 ms)
                     0.994 R²   (0.986 R² .. 0.999 R²)
mean                 6.640 ms   (6.574 ms .. 6.757 ms)
std dev              270.6 μs   (168.3 μs .. 473.4 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking Stencil/Average/Repa Parallel
time                 39.36 ms   (38.33 ms .. 40.58 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 38.15 ms   (37.18 ms .. 39.03 ms)
std dev              1.951 ms   (1.357 ms .. 2.454 ms)
variance introduced by outliers: 13% (moderately inflated)
```


Sum over an array with a left fold:

```
Benchmark fold-seq: RUNNING...
benchmarking Sum (1600x1200)/Sequential/Massiv Ix2 U
time                 1.860 ms   (1.850 ms .. 1.877 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.869 ms   (1.861 ms .. 1.886 ms)
std dev              35.77 μs   (20.65 μs .. 62.14 μs)

benchmarking Sum (1600x1200)/Sequential/Vector U
time                 1.690 ms   (1.686 ms .. 1.694 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.686 ms   (1.679 ms .. 1.692 ms)
std dev              20.98 μs   (16.14 μs .. 27.77 μs)

benchmarking Sum (1600x1200)/Sequential/Repa DIM2 U
time                 40.02 ms   (38.05 ms .. 42.81 ms)
                     0.993 R²   (0.987 R² .. 1.000 R²)
mean                 38.40 ms   (38.03 ms .. 39.44 ms)
std dev              1.225 ms   (303.9 μs .. 2.218 ms)

benchmarking Sum (1600x1200)/Parallel/Massiv Ix2 U
time                 751.3 μs   (744.1 μs .. 758.7 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 750.7 μs   (741.7 μs .. 762.3 μs)
std dev              32.13 μs   (19.02 μs .. 50.21 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking Sum (1600x1200)/Parallel/Repa DIM2 U
time                 9.581 ms   (9.415 ms .. 9.803 ms)
                     0.994 R²   (0.988 R² .. 0.998 R²)
mean                 9.085 ms   (8.871 ms .. 9.281 ms)
std dev              584.2 μs   (456.4 μs .. 800.4 μs)
variance introduced by outliers: 34% (moderately inflated)

Benchmark fold-seq: FINISH
```

Here is also a recent addition `mapM` over an array. Comparing only with unboxed vectors, since Repa
doesn't have such an implementation. There is 100x speedup over `vector` package since direct
mutation is used versus a fallback onto lists, like it is done with @Vector@:

```
benchmarking mapM/Array Ix2 P Seq
time                 2.408 ms   (2.346 ms .. 2.464 ms)
                     0.994 R²   (0.991 R² .. 0.997 R²)
mean                 2.464 ms   (2.417 ms .. 2.532 ms)
std dev              183.6 μs   (135.2 μs .. 236.3 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking mapM/Array Ix2 U Seq
time                 5.858 ms   (5.508 ms .. 6.148 ms)
                     0.979 R²   (0.961 R² .. 0.989 R²)
mean                 5.627 ms   (5.476 ms .. 5.812 ms)
std dev              476.9 μs   (383.0 μs .. 621.8 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking mapM/Vector U
time                 269.6 ms   (216.1 ms .. 317.4 ms)
                     0.989 R²   (0.973 R² .. 1.000 R²)
mean                 272.2 ms   (261.7 ms .. 281.6 ms)
std dev              11.88 ms   (5.525 ms .. 15.30 ms)
variance introduced by outliers: 16% (moderately inflated)
```
