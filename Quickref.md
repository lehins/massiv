# Massiv Quick Reference

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
