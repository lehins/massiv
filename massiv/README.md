# massiv

`massiv` is a Haskell library for array manipulation. Performance is one of its main goals, thus it
is capable of seamless parallelization of most of the operations provided by the library

The name for this library comes from the Russian word Massiv (Масси́в), which means an Array.

## Status

| Language | Github Actions | Coveralls | Gitter.im |
|:--------:|:--------------:|:---------:|:---------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/massiv.svg) | [![Build Status](https://github.com/lehins/massiv/workflows/massiv-CI/badge.svg)](https://github.com/lehins/massiv/actions) | [![Coverage Status](https://coveralls.io/repos/github/lehins/massiv/badge.svg?branch=master)](https://coveralls.io/github/lehins/massiv?branch=master) | [![Join the chat at https://gitter.im/haskell-massiv/Lobby](https://badges.gitter.im/haskell-massiv/Lobby.svg)](https://gitter.im/haskell-massiv/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

|      Package       | Hackage | Nightly | LTS |
|:-------------------|:-------:|:-------:|:---:|
|  [`massiv`](https://github.com/lehins/massiv/tree/master/massiv)|                                       [![Hackage](https://img.shields.io/hackage/v/massiv.svg)](https://hackage.haskell.org/package/massiv)|                                                                                                        [![Nightly](https://www.stackage.org/package/massiv/badge/nightly)](https://www.stackage.org/nightly/package/massiv)| ](https://www.stackage.org/package/massiv-io/badge/lts)](https://www.stackage.org/lts/package/massiv-io)|
|  [`massiv-test`](https://github.com/lehins/massiv/tree/master/massiv-test)|                            [![Hackage](https://img.shields.io/hackage/v/massiv-test.svg)](https://hackage.haskell.org/package/massiv-test)|                                                                                              [![Nightly](https://www.stackage.org/package/massiv-test/badge/nightly)](https://www.stackage.org/nightly/package/massiv-test)|                                                                               [![Nightly](https://www.stackage.org/package/massiv-test/badge/lts)](https://www.stackage.org/lts/package/massiv-test)|
|  [`haskell-scheduler`](https://github.com/lehins/haskell-scheduler)|                                   [![Hackage](https://img.shields.io/hackage/v/scheduler.svg)](https://hackage.haskell.org/package/scheduler)|                                                                                          [![Nightly](https://www.stackage.org/package/scheduler/badge/nightly)](https://www.stackage.org/nightly/package/scheduler)|                                                                   [![Nightly](https://www.stackage.org/package/scheduler/badge/lts)](https://www.stackage.org/lts/package/scheduler)|

## Introduction

Everything in the library revolves around an `Array r ix e` - a data family for anything that can be
thought of as an array. The type variables, from the end, are:

* `e` - element of an array.
* `ix` - an index that will map to an actual element. The index must be an instance of the `Index`
  class with the default one being an `Ix n` type family and an optional being tuples of `Int`s.
* `r` - underlying representation. There are two main categories of representations described below.

### Manifest

These are your classical arrays that are located in memory and allow constant time lookup of
elements. Another main property they share is that they have a mutable interface. An `Array` with
manifest representation can be thawed into a mutable `MArray` and then frozen back into its
immutable counterpart after some destructive operation is applied to the mutable copy. The
differences among representations below is in the way that elements are being accessed in memory:

  * `P` - Array with elements that are an instance of `Prim` type class, i.e. common Haskell
    primitive types: `Int`, `Word`, `Char`, etc. It is backed by unpinned memory and based on
    [`ByteArray`](https://hackage.haskell.org/package/primitive/docs/Data-Primitive-ByteArray.html#t:ByteArray).
  * `U` - Unboxed arrays. The elements are instances of the
    [`Unbox`](https://hackage.haskell.org/package/vector/docs/Data-Vector-Unboxed.html#t:Vector)
    type class. Usually just as fast as `P`, but has a slightly wider range of data types that it
    can work with. Notable data types that can be stored as elements are `Bool`, tuples and `Ix n`.
  * `S` - Storable arrays. Backed by pinned memory and based on `ForeignPtr`, while elements are
    instances of the `Storable` type class.
  * `B` - Boxed arrays that don't have restrictions on their elements, since they are represented
    as pointers to elements, thus making them the slowest type of array, but also the most
    general. Arrays of this representation are element strict, in other words its elements are
    kept in Weak-Head Normal Form (WHNF).
  * `BN` - Also boxed arrays, but unlike the other representation `B`, its elements are in Normal
    Form, i.e. in a fully evaluated state and no thunks or memory leaks are possible. It does
    require an `NFData` instance for the elements though.
  * `BL` - Boxed lazy array. Just like `B` and `BN`, except values are evaluated on demand.

### Delayed

Main trait of delayed arrays is that they do not exist in memory and instead describe the contents
of an array as a function or a composition of functions. In fact all of the fusion capabilities in
`massiv` can be attributed to delayed arrays.

   * `D` - Delayed "pull" array is just a function from an index to an element: `(ix ->
     e)`. Therefore indexing into this type of array is not possible, instead elements are evaluated
     with the `evaluateM` function each time when applied to an index. It gives us a nice ability to
     compose functions together when applied to an array and possibly even fold over without ever
     allocating intermediate manifest arrays.
   * `DW` - Delayed windowed array is very similar to the version above, except it has two functions
     that describe it, one for the near border elements and one for the interior, aka. the
     window. This is used for [`Stencil`](stencil) computation and things that derive from it, such as
     convolution, for instance.
   * `DL` - Delayed "push" array contains a monadic action that describes how an array can be loaded
     into memory. This is most useful for composing arrays together.
   * `DS` - Delayed stream array is a sequence of elements, possibly even an infinite one. This is
     most useful for situations when we don't know the size of our resulting array ahead of time,
     which is common in operations such as `filter`, `mapMaybe`, `unfold` etc. Naturally, in the end
     we can only load such an array into a flat vector.
   * `DI` - Is just like `D`, except loading is interleaved and is useful for parallel loading
     arrays with unbalanced computation, such as Mandelbrot set or ray tracing, for example.

## Construct

Creating a delayed type of array allows us to fuse any future operations we decide to perform on
it. Let's look at this example:

```haskell
λ> import Data.Massiv.Array as A
λ> makeVectorR D Seq 10 id
Array D Seq (Sz1 10)
  [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
```

Here we created a delayed vector of size 10, which is in reality just an `id` function from its
index to an element (see the [Computation](#computation) section for the meaning of `Seq`). So let's
go ahead and square its elements

```haskell
λ> makeVectorR D Seq 10 id
λ> evaluateM vec 4
4
λ> vec2 = A.map (^ (2 :: Int)) vec
λ> evaluateM vec2 4
16
```

It's not that exciting, since every time we call `evaluateM` it will recompute the element, __every
time__, therefore this function should be avoided at all costs! Instead we can use all of the
functions that take `Source` like arrays and then fuse that computation together by calling
`compute`, or a handy `computeAs` function and only afterwards apply an `indexM` function or its
partial synonym: `(!)`. Any delayed array can also be reduced using one of the folding functions,
thus completely avoiding any memory allocation, or converted to a list, if that's what you need:

```haskell
λ> vec2U = computeAs U vec2
λ> vec2U
Array U Seq (Sz1 10)
  [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ]
λ> vec2U ! 4
16
λ> toList vec2U
[0,1,4,9,16,25,36,49,64,81]
λ> A.sum vec2U
285
```

There is a whole multitude of ways to construct arrays:
 * by using one of many helper functions: `makeArray`, `range`, `rangeStepFrom`, `enumFromN`, etc.
 * through conversion: from lists, from `Vector`s in `vector` library, from `ByteString`s in
   `bytestring`;
 * with a mutable interface in `PrimMonad` (`IO`, `ST`, etc.), eg: `makeMArray`,
   `generateArray`, `unfoldrPrim`, etc.

It's worth noting that, in the next example, nested lists will be loaded into an unboxed manifest
array and the sum of its elements will be computed in parallel on all available cores.

```haskell
λ> A.sum (fromLists' Par [[0,0,0,0,0],[0,1,2,3,4],[0,2,4,6,8]] :: Array U Ix2 Double)
30.0
```

The above wouldn't run in parallel in ghci of course, as the program would have to be compiled with
`ghc` using `-threaded -with-rtsopts=-N` flags in order to use all available cores. Alternatively we
could compile with the `-threaded` flag and then pass the number of capabilities directly to the
runtime with `+RTS -N<n>`, where `<n>` is the number of cores you'd like to utilize.

## Index

The main `Ix n` closed type family can be somewhat confusing, but there is no need to fully
understand how it works in order to start using it. GHC might ask you for the `DataKinds` language
extension if `IxN n` is used in a type signature, but there are type and pattern synonyms for the
first five dimensions: `Ix1`, `Ix2`, `Ix3`, `Ix4` and `Ix5`.

There are three distinguishable constructors for the index:

* The first one is simply an int: `Ix1 = Ix 1 = Int`, therefore vectors can be indexed in a usual way
  without some extra wrapping data type, just as it was demonstrated in a previous section.
* The second one is `Ix2` for operating on 2-dimensional arrays and has a constructor `:.`

```haskell
λ> makeArrayR D Seq (Sz (3 :. 5)) (\ (i :. j) -> i * j)
Array D Seq (Sz (3 :. 5))
  [ [ 0, 0, 0, 0, 0 ]
  , [ 0, 1, 2, 3, 4 ]
  , [ 0, 2, 4, 6, 8 ]
  ]
```

* The third one is `IxN n` and is designed for working with N-dimensional arrays, and has a similar
  looking constructor `:>`, except that it can be chained indefinitely on top of `:.`

```haskell
λ> arr3 = makeArrayR P Seq (Sz (3 :> 2 :. 5)) (\ (i :> j :. k) -> i * j + k)
λ> :t arr3
arr3 :: Array P (IxN 3) Int
λ> arr3
Array P Seq (Sz (3 :> 2 :. 5))
  [ [ [ 0, 1, 2, 3, 4 ]
    , [ 0, 1, 2, 3, 4 ]
    ]
  , [ [ 0, 1, 2, 3, 4 ]
    , [ 1, 2, 3, 4, 5 ]
    ]
  , [ [ 0, 1, 2, 3, 4 ]
    , [ 2, 3, 4, 5, 6 ]
    ]
  ]
λ> arr3 ! (2 :> 1 :. 4)
6
λ> ix10 = 10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1
λ> :t ix10
ix10 :: IxN 10
λ> ix10 -- 10-dimensional index
10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1
```

Here is how we can construct a 4-dimensional array and sum its elements in constant memory:

```haskell
λ> arr = makeArrayR D Seq (Sz (10 :> 20 :> 30 :. 40)) $ \ (i :> j :> k :. l) -> (i * j + k) * k + l
λ> :t arr -- a 4-dimensional array
arr :: Array D (IxN 4) Int
λ> A.sum arr
221890000
```

There are quite a few helper functions that can operate on indices, but these are only needed when
writing functions that work for arrays of arbitrary dimension, as such they are scarcely used:

```haskell
λ> pullOutDim' ix10 5
(5,10 :> 9 :> 8 :> 7 :> 6 :> 4 :> 3 :> 2 :. 1)
λ> unconsDim ix10
(10,9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :> 2 :. 1)
λ> unsnocDim ix10
(10 :> 9 :> 8 :> 7 :> 6 :> 5 :> 4 :> 3 :. 2,1)
```

All of the `Ix n` indices are instances of `Num` so basic numeric operations are made easier:

```haskell
λ> (1 :> 2 :. 3) + (4 :> 5 :. 6)
5 :> 7 :. 9
λ> 5 :: Ix4
5 :> 5 :> 5 :. 5
```

It is important to note that the size type is distinct from the index by the newtype wrapper `Sz
ix`. There is a constructor `Sz`, which will make sure that none of the dimensions are negative:

```haskell
λ> Sz (2 :> 3 :. 4)
Sz (2 :> 3 :. 4)
λ> Sz (10 :> 2 :> -30 :. 4)
Sz (10 :> 2 :> 0 :. 4)
```

Same as with indices, there are helper pattern synonyms: `Sz1`, `Sz2`, `Sz3`, `Sz4` and `Sz5`.

```haskell
λ> Sz3 2 3 4
Sz (2 :> 3 :. 4)
λ> Sz4 10 2 (-30) 4
Sz (10 :> 2 :> 0 :. 4)
```

As well as the `Num` instance:

```haskell
λ> 4 :: Sz5
Sz (4 :> 4 :> 4 :> 4 :. 4)
λ> (Sz2 1 2) + 3
Sz (4 :. 5)
λ> (Sz2 1 2) - 3
Sz (0 :. 0)
```

Alternatively tuples of `Int`s can be used for working with arrays, up to and including 5-tuples
(type synonyms: `Ix2T` .. `Ix5T`), but since tuples are polymorphic it is necessary to restrict the
resulting array type. Not all operations in the library support tuples, so it is advised to avoid
them for indexing.

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

There are helper functions that can go back and forth between tuples and `Ix n` indices.

```haskell
λ> fromIx4 (3 :> 4 :> 5 :. 6)
(3,4,5,6)
λ> toIx5 (3, 4, 5, 6, 7)
3 :> 4 :> 5 :> 6 :. 7
```

## Slicing

In order to get a subsection of an array there is no need to recompute it, unless we want to free up
the no longer memory, of course. So, there are a few slicing, resizing and extraction operators that
can do it all in constant time, modulo the index manipulation:

```haskell
λ> arr = makeArrayR U Seq (Sz (4 :> 2 :. 6)) fromIx3
λ> arr !> 3 !> 1
Array M Seq (Sz1 6)
  [ (3,1,0), (3,1,1), (3,1,2), (3,1,3), (3,1,4), (3,1,5) ]
```

As you might suspect all of the slicing, indexing, extracting, resizing operations are partial, and
those are frowned upon in Haskell. So there are matching functions that can do the same operations
safely by using `MonadThrow` and thus returning `Nothing`, `Left SomeException` or throwing an
exception in case of `IO` on failure, for example:

```haskell
λ> arr !?> 3 ??> 1
Array M Seq (Sz1 6)
  [ (3,1,0), (3,1,1), (3,1,2), (3,1,3), (3,1,4), (3,1,5) ]
λ> arr !?> 3 ??> 1 ?? 0 :: Maybe (Int, Int, Int)
Just (3,1,0)
```

In above examples we first take a slice at the 4th page (index 3, since we start at 0), then another
one at the 2nd row (index 1). While in the last example we also take 1st element at
position 0. Pretty neat, huh?  Naturally, by doing a slice we always reduce dimension by one. We can
do slicing from the outside as well as from the inside:

```haskell
λ> Ix1 1 ... 9
Array D Seq (Sz1 10)
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
λ> a <- resizeM (Sz (3 :> 2 :. 4)) $ Ix1 11 ... 34
λ> a
Array D Seq (Sz (3 :> 2 :. 4))
  [ [ [ 11, 12, 13, 14 ]
    , [ 15, 16, 17, 18 ]
    ]
  , [ [ 19, 20, 21, 22 ]
    , [ 23, 24, 25, 26 ]
    ]
  , [ [ 27, 28, 29, 30 ]
    , [ 31, 32, 33, 34 ]
    ]
  ]
λ> a !> 0
Array D Seq (Sz (2 :. 4))
  [ [ 11, 12, 13, 14 ]
  , [ 15, 16, 17, 18 ]
  ]
λ> a <! 0
Array D Seq (Sz (3 :. 2))
  [ [ 11, 15 ]
  , [ 19, 23 ]
  , [ 27, 31 ]
  ]
```

Or we can slice along any other available dimension:

```haskell
λ> a <!> (Dim 2, 0)
Array D Seq (Sz (3 :. 4))
  [ [ 11, 12, 13, 14 ]
  , [ 19, 20, 21, 22 ]
  , [ 27, 28, 29, 30 ]
  ]
```

In order to extract sub-array while preserving dimensionality we can use `extractM` or `extractFromToM`.

```haskell
λ> extractM (0 :> 1 :. 1) (Sz (3 :> 1 :. 2)) a
Array D Seq (Sz (3 :> 1 :. 2))
  [ [ [ 16, 17 ]
    ]
  , [ [ 24, 25 ]
    ]
  , [ [ 32, 33 ]
    ]
  ]
λ> extractFromToM (1 :> 0 :. 1) (3 :> 2 :. 4) a
Array D Seq (Sz (2 :> 2 :. 3))
  [ [ [ 20, 21, 22 ]
    , [ 24, 25, 26 ]
    ]
  , [ [ 28, 29, 30 ]
    , [ 32, 33, 34 ]
    ]
  ]
```

## Computation and parallelism

There is a data type `Comp` that controls how elements will be computed when calling the `compute`
function. It has a few constructors, although most of the time either `Seq` or `Par` will be
sufficient:

* `Seq` - computation will be done sequentially on one core (capability in ghc).
* `ParOn [Int]` - perform computation in parallel while pinning the workers to particular
  cores. Providing an empty list will result in the computation being distributed over all
  available cores, or better known in Haskell as capabilities.
* `ParN Word16` - similar to `ParOn`, except it simply specifies the number of cores to
  use, with `0` meaning all cores.
* `Par` - isn't really a constructor but a `pattern` for constructing `ParOn []`, which
  will result in Scheduler using all cores, thus should be used instead of `ParOn`.
* `Par'` - similar to `Par`, except it uses `ParN 0` underneath.

Just to make sure a simple novice mistake is prevented, which I have seen in the past, make sure
your source code is compiled with `ghc -O2 -threaded -with-rtsopts=-N`, otherwise no parallelization
and poor performance are waiting for you. Also a bit later you might notice the `{-# INLINE funcName
#-}` pragma being used, oftentimes it is a good idea to do that, but not always required. It is
worthwhile to benchmark and experiment.

## Stencil

Instead of manually iterating over a multi-dimensional array and applying a function to each element,
while reading its neighboring elements (as you would do in an imperative language) in a functional
language it is much more efficient to apply a stencil function and let the library take care of all
of bounds checking and iterating in a cache friendly manner.

What's a [stencil](https://en.wikipedia.org/wiki/Stencil_code)? It is a declarative way of
specifying a pattern for how elements of an array in a neighborhood will be used in order to update
each element of the newly created array. In massiv a `Stencil` is a function that can read the
neighboring elements of the stencil's _center_ (the zero index), and only those, and then outputs a
new value for the center element.

![stencil](massiv-examples/files/stencil.png)

Let's create a simple, but somewhat meaningful array and create an averaging stencil. There is
nothing special about the array itself, but the averaging filter is a stencil that sums the elements
in a [Moore neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) and divides the result
by 9, i.e. finds the average of a 3 by 3 square.

```haskell
arrLightIx2 :: Comp -> Sz Ix2 -> Array D Ix2 Double
arrLightIx2 comp arrSz = makeArray comp arrSz $ \ (i :. j) -> sin (fromIntegral (i * i + j * j))
{-# INLINE arrLightIx2 #-}

average3x3Filter :: Fractional a => Stencil Ix2 a a
average3x3Filter = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}
```

Here is what it would look like in GHCi. We create a delayed array with some funky periodic
function, and make sure it is computed prior to mapping an average stencil over it:

```haskell
λ> arr = computeAs U $ arrLightIx2 Par (Sz (600 :. 800))
λ> :t arr
arr :: Array U Ix2 Double
λ> :t mapStencil Edge average3x3Filter arr
mapStencil Edge average3x3Filter arr :: Array DW Ix2 Double
```

As you can see, that operation produced an array of the earlier mentioned representation Delayed
Windowed `DW`. In its essence `DW` is an array type that does no bounds checking in order to gain
performance, except when it's near the border, where it uses a border resolution technique supplied
by the user (`Edge` in the example above). Currently it is used only in stencils and not much else
can be done to an array of this type besides further computing it into a manifest representation.

This example will be continued in the next section, but before that I would like to mention that
some might notice that it looks very much like convolution, and in fact convolution can be
implemented with a stencil. There is a helper function `makeConvolutionStencil` that lets
you do just that. For the sake of example we'll do a sum of all neighbors by hand instead:

```haskell
sum3x3Filter :: Fractional a => Stencil Ix2 a a
sum3x3Filter = makeConvolutionStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE sum3x3Filter #-}
```

There is not a single plus or multiplication sign, that is because convolutions is actually
summation of elements multiplied by a kernel element, so instead we have composition of functions
applied to an offset index and a multiplier. After we map that stencil, we can further divide each
element of the array by 9 in order to get the average. Yeah, I lied a bit, `Array DW ix` is an
instance of `Functor` class, so we can map functions over it, which will be fused as with a regular
`D`elayed array:

```haskell
computeAs U $ fmap (/9) $ mapStencil Edge sum3x3Filter arr
```

If you are still confused of what a stencil is, but you are familiar with [Conway's Game of
Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) this should hopefully clarify it a
bit more. The function `life` below is a single iteration of Game of Life:

```haskell
lifeRules :: Word8 -> Word8 -> Word8
lifeRules 0 3 = 1
lifeRules 1 2 = 1
lifeRules 1 3 = 1
lifeRules _ _ = 0

lifeStencil :: Stencil Ix2 Word8 Word8
lifeStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
  lifeRules (get (0 :. 0)) $ get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
                             get ( 0 :. -1)         +         get ( 0 :. 1) +
                             get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)

life :: Array S Ix2 Word8 -> Array S Ix2 Word8
life = compute . mapStencil Wrap lifeStencil
```

<!-- TODO: add a gif with a few iterations -->

The full working example that uses GLUT and OpenGL is located in
[GameOfLife](massiv-examples/GameOfLife/app/GameOfLife.hs). You can run it if you have the GLUT
dependencies installed:

```bash
$ cd massiv-examples && stack run GameOfLife
```

# massiv-io

In order to do anything useful with arrays we often need to be able to read some data from a
file. Considering that most common array-like files are images,
[massiv-io](https://github.com/lehins/massiv-io) provides an interface to read, write and display
images in common formats using Haskell native JuicyPixels and Netpbm packages.

[Color](https://github.com/lehins/Color) package provides a variety of color spaces and conversions
between them, which are used by `massiv-io` package as pixels during reading and writing images.

An earlier example wasn't particularly interesting, since we couldn't visualize what is actually
going on, so let's expand on it:

```haskell
import Data.Massiv.Array
import Data.Massiv.Array.IO

main :: IO ()
main = do
  let arr = computeAs S $ arrLightIx2 Par (600 :. 800)
      toImage ::
           (Functor (Array r Ix2), Load r Ix2 (Pixel (Y' SRGB) Word8))
        => Array r Ix2 Double
        -> Image S (Y' SRGB) Word8
      toImage = computeAs S . fmap (PixelY' . toWord8)
      lightPath = "files/light.png"
      lightImage = toImage $ delay arr
      lightAvgPath = "files/light_avg.png"
      lightAvgImage = toImage $ mapStencil Edge (avgStencil 3) arr
      lightSumPath = "files/light_sum.png"
      lightSumImage = toImage $ mapStencil Edge (sumStencil 3) arr
  writeImage lightPath lightImage
  putStrLn $ "written: " ++ lightPath
  writeImage lightAvgPath lightAvgImage
  putStrLn $ "written: " ++ lightAvgPath
  writeImage lightSumPath lightSumImage
  putStrLn $ "written: " ++ lightSumPath
  displayImageUsing defaultViewer True . computeAs S
    =<< concatM 1 [lightAvgImage, lightImage, lightSumImage]
```

`massiv-examples/vision/files/light.png`:

![Light](massiv-examples/vision/files/light.png)

`massiv-examples/vision/files/light_avg.png`:

![Light Average](massiv-examples/vision/files/light_avg.png)


The full example is in the example [vision](massiv-examples/vision/app/AvgSum.hs) package and if you
have `stack` installed you can run it as:

```bash
$ cd massiv-examples && stack run avg-sum
```

# Other libraries

A natural question might come to mind: Why even bother with a new array library when we already have
a few really good ones in the Haskell world? The main reasons for me are performance and
usability. I personally felt like there was much room for improvement before I even started working on
this package, and it seems like it turned out to be true. For example, the most common goto library
for dealing with multidimensional arrays and parallel computation used to be
[Repa](https://hackage.haskell.org/package/repa), which I personally was a big fan of for quite some
time, to the point that I even wrote a [Haskell Image
Processing](https://hackage.haskell.org/package/hip) library based on top of it.

Here is a quick summary of how `massiv` is better than `Repa`:

* It is actively maintained.
* Much more sophisticated scheduler. It is resumable and is capable of handling nested parallel
  computation.
* Improved indexing data types.
* Safe stencils for arbitrary dimensions, not only 2D convolution. Stencils are composable
* Improved performance on almost all operations.
* Structural parallel folds (i.e. left/right - direction is preserved)
* Super easy slicing.
* Extensive mutable interface
* More fusion capabilities with delayed stream and push array representations.
* Delayed arrays aren't indexable, only Manifest are (saving user from common pitfall in Repa of
  trying to read elements of delayed array)

As far as usability of the library goes, it is very subjective, thus I'll let you be a judge of
that. When talking about performance it is the facts that do matter. Thus, let's not continue this
discussion in pure abstract words, below is a glimpse into benchmarks against Repa library running
with GHC 8.8.4 on Intel® Core™ i7-3740QM CPU @ 2.70GHz × 8

[Matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication_algorithm):

```
benchmarking Repa/MxM U Double - (500x800 X 800x500)/Par
time                 120.5 ms   (115.0 ms .. 127.2 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 124.1 ms   (121.2 ms .. 127.3 ms)
std dev              5.212 ms   (2.422 ms .. 6.620 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking Massiv/MxM U Double - (500x800 X 800x500)/Par
time                 41.46 ms   (40.67 ms .. 42.45 ms)
                     0.998 R²   (0.994 R² .. 0.999 R²)
mean                 38.45 ms   (37.22 ms .. 39.68 ms)
std dev              2.342 ms   (1.769 ms .. 3.010 ms)
variance introduced by outliers: 19% (moderately inflated)
```

[Sobel operator](https://en.wikipedia.org/wiki/Sobel_operator):
```
benchmarking Sobel/Par/Operator - Repa
time                 17.82 ms   (17.30 ms .. 18.32 ms)
                     0.997 R²   (0.994 R² .. 0.998 R²)
mean                 17.42 ms   (17.21 ms .. 17.69 ms)
std dev              593.0 μs   (478.1 μs .. 767.5 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Sobel/Par/Operator - Massiv
time                 7.421 ms   (7.230 ms .. 7.619 ms)
                     0.994 R²   (0.991 R² .. 0.997 R²)
mean                 7.537 ms   (7.422 ms .. 7.635 ms)
std dev              334.3 μs   (281.3 μs .. 389.9 μs)
variance introduced by outliers: 20% (moderately inflated)
```

Sum all elements of a 2D array:

```
benchmarking Sum/Seq/Repa
time                 539.7 ms   (523.2 ms .. 547.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 540.1 ms   (535.7 ms .. 543.2 ms)
std dev              4.727 ms   (2.208 ms .. 6.609 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Sum/Seq/Vector
time                 16.95 ms   (16.78 ms .. 17.07 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 17.23 ms   (17.13 ms .. 17.43 ms)
std dev              331.4 μs   (174.1 μs .. 490.0 μs)

benchmarking Sum/Seq/Massiv
time                 16.78 ms   (16.71 ms .. 16.85 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.80 ms   (16.76 ms .. 16.88 ms)
std dev              127.8 μs   (89.95 μs .. 186.2 μs)

benchmarking Sum/Par/Repa
time                 81.76 ms   (78.52 ms .. 84.37 ms)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 79.20 ms   (78.03 ms .. 80.91 ms)
std dev              2.613 ms   (1.565 ms .. 3.736 ms)

benchmarking Sum/Par/Massiv
time                 8.102 ms   (7.971 ms .. 8.216 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 7.967 ms   (7.852 ms .. 8.028 ms)
std dev              236.4 μs   (168.4 μs .. 343.2 μs)
variance introduced by outliers: 11% (moderately inflated)
```

Here is also a blog post that compares [Performance of Haskell Array libraries through Canny edge detection](https://alexey.kuleshevi.ch/blog/2020/07/10/canny-benchmarks/)

# Further resources on learning `massiv`:

* [2021 - Haskell eXchange - Multi-dimensional Arrays that Do Not Exist](#2021---haskell-exchange---multi-dimensional-arrays-that-do-not-exist)
* [2019 - Monadic Party - Haskell arrays with Massiv](https://github.com/lehins/talks#2019---monadic-party---haskell-arrays-with-massiv)
* [2018 - Monadic Warsaw #14 - Haskell arrays that are easy and fast](https://github.com/lehins/talks#2018---monadic-warsaw-14---haskell-arrays-that-are-easy-and-fast)
