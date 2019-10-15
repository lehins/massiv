# Massiv Quick Reference

## Indices

Everyone is well accustomed to the fact that the order of indices corresponds to the number of
dimensions an array can have in the reverse order, eg in `C`: `arr[i][j][k]` will mean that a
3-dimensional array is indexed at an outer most 3rd dimension with index `i`, 2nd dimension `j` and
the inner most 1st dimension `k`. In case of a 3d world `i` points to a page, `j` to a column and
`k` to the row, but the astraction scales pretty well to any dimension as long as we agree on the
order of things. Below are two ways to index an array in massiv:

```haskell
λ> arr = makeArrayR U Seq (Sz (2 :> 3 :. 4)) $ \ (i :> j :. k) -> i + j ^ k
λ> arr ! 1 :> 2 :. 3
9
λ> arr !> 1 !> 2
(Array M Seq (Sz1 (4))
  [ 2,3,5,9 ])
λ> arr !> 1 !> 2 !> 3
9
```

Former does the lookup of an element in the array, while the latter slices the array until it gets to
the actual element. Normally they are equivalent, but since implemnetation i svastly different,
difference in performance could be expected.

Most important thing to agree upon is the fact that at the end of the day we do represent data in a
linear row-major fashion, so the above indexing technique translates into a linear index that will
get mapped into an element in memory at some point.


## Hierarchy

### Class dependency

```
                                       Construct (D, DL, DS, DI, DW, B, N, P, U, S, LN) -> Ragged (L)
                                                           \
Load (DL, DS, DI, DW, L, LN) -> Source (D) -> Manifest (M) -`-> Mutable (B, N, P, U, S)
   |\
   | `> StrideLoad (D, DI, DW, M, B, N, P, U, S)
   |\
   | `> Extract (D, DS, DI, M, B, N, P, U, S)
   |\
   | `> Slice (D, M, B, N, P, U, S)
   |\
   | `> OuterSlice (D, M, B, N, P, U, S, L)
    \
     `> InnerSlice (D, M, B, N, P, U, S)

Stream (D, DS, B, N, P, U, S, L, LN)

Resize (D, DL, DI, B, N, P, U, S)
```

## Computation

As you know arrays can be computed in parallel or sequentially in `massiv`, but there is a lot more
to that:

* Normally you can supply computation strategy as an argument (`Seq`/`Par`) during the array
  construction or conversion, eg. from a list or vector
* array computation strategy will be combined according to its `Monoid` instance when two or more
  arrays are being joined together by some operation into another one.
* Most of functions will respect the inner computation strategy, while other will ignore it due to
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
