# massiv

`massiv` is a Haskell library for array manipulation. The goal of this library
is to be fast and easy to use. Performance is one of it's main goals, thus it is
capable to effortlessly run almost all operations in parallel as well as
sequentially.

Name for this library comes from a Russian word Massiv (Масси́в), which means an
Array.

__Disclaimer__: Current status of this library is under development, so anything
in it's API is subject to change. That being said it is already pretty
functional with some decent testing in place, so it is at a great stage to be
experimented with, at your own risk, of course.

## Introduction

Everything in the library revolves around an `Array r ix e` - a data type family
for anything that can be thought of as an array. The type variables, form the
end are:

* `e` - element of an array.
* `ix` - an index that will map to an actual element. Index must be an instance of
  `Index` class with default one being an `Ix n` type family and an optional
  being tuples of `Int`s.
* `r` - underlying representation. Main representations are:

    * `D` - delayed array, which is simply a function from an index to an
      element: `(ix -> e)`. Therefore indexing of this type of array is not
      possible, although elements can be computed with `evaluateAt` function.
    * `P` - Array with elements that are an instance of `Prim` type class,
      i.e. common Haskell primitive types: `Int`, `Word`, `Char`, etc. Backed by
      the usual `ByteArray`.
    * `U` - Unboxed arrays. Elements have instances of `Unbox` class. Just as
      fast as `P`, but has a much wider range of data types that it can work
      with.
    * `S` - Storable arrays. Backed by pinned `ByteArray`s and elements are
      instances of `Storable` class.
    * `B` - Boxed arrays that don't have restriction on it's elements, since
      they are represented as pointers to elements, thus making them the slowest
      type of array.
    * `M` - Manifest arrays, which is a general type of array that is backed by
      some memory representation, therefore any of the above `P`, `U`, `S`, `B`
      type of arrays can be converted to `M` in constant time with `toManifest`
      function. It is mostly useful during constant time slicing of manifest
      arrays, as this becomes the result representation. More on that
      in [slicing](#slicing) section.

## Construct

Creating a delayed type of array allows us to fuse any future operation we decide
to perform on it. Let's look at this example:

```haskell
λ> import Data.Array.Massiv as M
λ> let vec = makeVectorR D Seq 10 id
λ> :t vec
vec :: Array D Int Int
```

Here we created a delayed vector, which is in reality just an `id` function from
it's index to an element. So let's go ahead and square it's elements

```haskell
λ> evaluateAt vec 4
4
λ> let vec2 = fmap (^2) vec
λ> evaluateAt vec2 4
16
```

It's not that exciting, since every time we call `evaluateAt` it will recompute
the element, __every time__, therefore this function should be avoided at all
costs. Instead we can use all of the functions that take `Source` like arrays
and then fused that computation together by calling `compute`, or a handy
`computeAs` function and only afterwards apply an `index` function or it's
synonym: `(!)`. Any delayed array can also be reduced using one of the folding
functions, thus completely avoiding any memory allocation, or converted to a
list, if that's what you need:

```haskell
λ> let vec2U = computeAs U vec2
λ> :t vec2U
vec2U :: Array U Int Int
λ> vec2U ! 4
16
λ> toListIx1 vec2U
[0,1,4,9,16,25,36,49,64,81]
λ> M.sum vec2U
285
```

Other means of constructing arrays are through conversion from lists, vectors
from `vector` library and few other helper functions as `range`, `enumFromN`,
etc. Worth noting, that in the next example, nested list will be loaded in
parallel as well as further computation of the `sum` will be distributed amongst
available cores (that is if it would be compiled instead of executed in ghci):

```haskell
λ> M.sum (fromListIx2 Par [[0,0,0,0,0],[0,1,2,3,4],[0,2,4,6,8]] :: Array U Ix2 Double)
30.0
```

## Index

The main `Ix n` closed type family can be somewhat confusing, but there is no
need to fully understand how it is implemented in order to start using it. GHC
might ask you for the `DataKinds` language extension if `IxN n` be used in a
type signature.

There are three distinguishable constructors for the index:

* First one is simply an integer: `Ix1 = Ix 1 = Int`, therefore vectors can be indexed in
  a usual way without some extra wrapping data type, just as it was demonstrated
  in a previous section.
* Second one is `Ix2` for operating on 2-dimensional arrays and has a constructor `:.`

```haskell
λ> toListIx2 $ makeArrayR D Seq (3 :. 5) (\ (i :. j) -> i * j)
[[0,0,0,0,0],[0,1,2,3,4],[0,2,4,6,8]]
```

* Next one is `IxN n` and is for creating and indexing N-dimensional arrays, and
  has a similar looking constructor `:>`, except that it can be chained
  indefinitely on top of `:.`

```haskell
λ> :t makeArrayR D Seq (10 :> 20 :. 30) $ \ (i :> j :. k) -> i * j + k
makeArrayR D Seq (10 :> 20 :. 30) $ \ (i :> j :. k) -> i * j + k
  :: Array D (IxN 3) Int
λ> let arr = makeArrayR D Seq (10 :> 20 :> 30 :. 40) $ \ (i :> j :> k :. l) -> (i * j + k) * k + l
λ> :t arr -- a 4-dimensional array
arr :: Array D (IxN 4) Int
λ> M.sum arr
221890000
λ> :t (10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1) -- 10-dimensional index
(10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1) :: IxN 10
```

Alternatively tuples of `Int`s can be used for working with Arrays, up to and
including 5-tuples (type synonims: `Ix2T` - `Ix5T`), but since tuples are
polymorphic it is necessary to restrict the resulting array type:

```haskell
λ> makeArray Seq (10, 20) (uncurry (*)) :: Array D Ix2T Int
<Array D (10,20) (Int)>
λ> :i Ix2T
type Ix2T = (Int, Int)
```

## Slicing

In order to get a subsection of an array there is no need to recompute it,
unless we want to free up the extra memory, of course. So, there are a few
slicing, resizing and extraction operators that can do it all in constant time,
modulo the index manipulation:

```haskell
λ> let arr = computeAs U $ makeArrayR D Seq (10 :> 20 :. 30) $ \ (i :> j :. k) -> (i, j, k)
λ> :t arr !> 3 <! 2
arr !> 3 <! 2 :: Array M Ix1 (Int, Int, Int)
λ> arr !> 3 <! 2 ! 1
(3,1,2)
```

In above example we first take a slice at 3rd page, then at a 2nd column and
finaly we pull out the first element in that column. Pretty neat, hah?

As you might suspect those slicing and indexing operators are partial, and those
are frowned upon in Haskell, so we can do this in a much safer way:

```haskell
λ> :t arr !?> 3
arr !?> 3 :: Maybe (Array M Ix2 (Int, Int, Int))
λ> :t arr !?> 3 <? 2
arr !?> 3 <? 2 :: Maybe (Array M Ix1 (Int, Int, Int))
λ> arr !?> 3 <? 2 ? 1
Just (3,1,2)
λ> arr !?> 3 ?> 1 ? 2
Just (3,1,2)
```

## Computation

There is a data type `Comp`, that controls how elements will be computed when calling
`compute` function. It has two constructors:

* `Seq` - computation will be done sequentially on one core.
* `ParOn [Int]` - perform computation in parallel while pinning the workers to
  particular cores. Providing an empty list will result in computation being
  distributed over all available cores, or better known in Haskell as
  capabilities.
* `Par` - isn't really a constructor but a `pattern` for constructing `ParOn
  []`, thus should be used instead of `ParOn`.

Just to make sure a simple novice mistake is prevented, which I have seen in the
past, make sure your source code is compiled with
`ghc -O2 -threaded -rtsopts -with-rtsopts=-N`, otherwise no parallelization and
poor performance are waiting for you. Also a bit later you might notice
`{-# INLINE funcName #-}` pragma being used, often times it is a good idea to do
that, but not always required. It is worth to benchmark and experiment.

## Stencil

Instead of manually iterating over a multidimensional array and applying a
function to each element, while reading it's neighboring elements, as you would
do in an imperative language, in a functional language it is much more efficient
to apply a stencil function and let the library take care of all of bounds
checking and iterating in a cache friendly manner.

What's a stencil? It is a declarative way of specifying a pattern of how
elements of an array in a neighborhood will be used in order to update each
element of that array. To be more precise for this library it is a function that
possesses a way to index an element, which is assumed to be at a center of a
stencil, i.e. zero index, and it's neighbors using an offset indexing, but not
beyond the specified size with respect to the center index. A mouthful, will go
for an example as it is worth a thousand words.

Let's create a simple, but somewhat meaningful array and create an average
stencil. There is nothing super interesting about the array itself, but the
filter is a stencil that will sum the elements in
a [Moore neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) and
divide the result by 9, i.e. finds the average of a 3 by 3 square.

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
funky periodic function, make sure it is computed prior to mapping an average
stencil over it:

```haskell
λ> let arr = arrLightIx2 Par (600 :. 800)
λ> :t arr
arr :: Array D Ix2 Double
λ> :t mapStencil (average3x3Filter Edge) $ computeAs U arr
mapStencil (average3x3Filter Edge) $ computeAs U arr
  :: Array WD Ix2 Double
```

As you can see, that operation produced an array of some weird representation
`WD`, which stands for Windowed Delayed array. In it's essence `WD` is an array
type that does no bounds checking in order to gain performance, except when it's
near the border, where it uses a border resolution technique supplied by the
user, like `Edge` in the example above. Currently it is used only in stencils
and there is not much else can be done to an array of this type besides further
computing it into a manifest representation.

This example will be continued in the next section, but before that I would like
to mention that some might notice that it looks very much like convolution, and
in fact convolution can be implemented with a stencil, therefore here is a
helper function `mkConvolutionStencil` that let's you do just that. Although,
instead of going the easy route, for the sake of example we'll do a sum of all
neighbors:

```haskell
sum3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
sum3x3Filter b = mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE sum3x3Filter #-}
```

There is not a single plus, because that is summing elements is convolution, so
instead we have composition of functions applied to an offseet index and a
multiplier. After we map that stencil, we can further divide each element of the
array by 9 in order to get the average. Yeah, I lied a bit, `Array WD ix` is an
instance of a `Functor`, so we can map functions over it, which will be fused as
with a regular `D`elayed array:

```haskell
computeAs U $ fmap (/9) . mapStencil (sum3x3Filter Edge) $ computeAs U arr
```

# massiv-io

In order to do anything useful with arrays we need to be able to read some data
from a file. Considering that most common array-like files are images and
`massiv-io` provides an interface to read, write and display images in common
formats using Haskell native JuicyPixels and Netpbm packages.

There are also a variety of colorspaces (or rather color models) and pixel types
that are currently included in this package, which will likely find a separate
home in the future, but for now we can sort of ignore those color spaces and
pretend that `Pixel` is some magic wrapper around numeric values that this
package is capable of reading/writing.

Previous example wasn't particularly interesting, since we couldn't visualize
what is actually going on, so let's expend on it:

```haskell
main :: IO ()
main = do
  let arr = arrLightIx2 Par (600 :. 800)
      img = computeAs S $ fmap PixelY arr -- convert an array into a grayscale image
  writeImage "files/light.png" img
  writeImage "files/light_avg.png" $ computeAs S $ mapStencil (average3x3Filter Edge) img
```

`files/light.png`:

[!Light](files/light.png)

`files/light_avg.png`:

[!Light](files/light_avg.png)


The full example is in the root of the repository and if you have stack
installed you can run it as:

```bash
$ ./Examples.hs && ./Examples
```


# Other libraries

A natural question might come to mind: Why even bother with a new array library
when we already have a few really good ones in Haskell world? The main reasons
for me are performance and usability. I personally felt like there was much room
for improvement even before I started work on this package, and it seems as it
turned out to be true. For example, the most common goto library for dealing with
multidimensional arrays and parallel computation over them is Repa, which I
personally was a big fan of for the longest time, to the point that I even wrote
a [Haskell Image Processing](https://hackage.haskell.org/package/hip) library
based on Repa.

Here is a quick summary of how `massiv` compares to Repa so far:

* Better scheduler, that is capable of handling nested parallel computation.
* Still shape polymorphic, but with improved default indexing data types.
* Safe stencils for arbitrary dimensions, not only convolution. Stencils are
  composable through an instance of Applicative (currently there is performance
  regression on this point for the new index type, but that is in the works of
  being fixed)
* Improved performance on almost all operations. (I might be wrong here, but so
  far it looks promising and rigorous benchmarks are coming to prove the claim)
* Structural parallel folds (i.e. left/right - direction is preserved)
* Super easy slicing.
* Delayed arrays aren't indexable, only Manifest are (saving user from common
  pitfall in Repa of trying to read elements of delayed array)

As far as usability of the library goes, it is very subjective, thus I'll let
you be a judge of that. When talking about performance it is the facts that do
matter.  Thus, not continue this discussion in pure abstract words, below is a
glimpse into benchmarks against Repa library running with GHC 8.0.2 on Intel®
Core™ i7-3740QM CPU @ 2.70GHz × 8

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
time                 559.3 ps   (555.9 ps .. 565.3 ps)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 560.1 ps   (558.1 ps .. 563.5 ps)
std dev              9.664 ps   (5.124 ps .. 16.50 ps)
variance introduced by outliers: 28% (moderately inflated)

benchmarking Sum (1600x1200)/Parallel/Repa DIM2 U
time                 9.581 ms   (9.415 ms .. 9.803 ms)
                     0.994 R²   (0.988 R² .. 0.998 R²)
mean                 9.085 ms   (8.871 ms .. 9.281 ms)
std dev              584.2 μs   (456.4 μs .. 800.4 μs)
variance introduced by outliers: 34% (moderately inflated)

Benchmark fold-seq: FINISH
```
