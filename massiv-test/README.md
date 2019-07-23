# massiv-test

This package is designed for users of `massiv`, that would like to do some testing of
their code, while reusing functionality that has already been written for testing `massiv`
itsef. This library is still a work in progress, nevertheless it is at a fairly usable
state. Below is a list of use case for this package.

## QuickCheck generators

First and foremost this package provides `Arbitrary` and `CoArbitrary` instances for the
relevant types available in `massiv`, as well as few extra handy `newtype` wrappers that
can be very useful for writing property tests for libraries and applications that depends
on `massiv`.

## Reusable spec

Another important use case is for advanced users that came up with their own index types
or array representations and would like to run a standard set of specs on their instance
implementations. For example a custom `Index ix`, or `Mutable r ix e` instances can use a
predefined collection of `hspec` specs and/or `QuickCheck` properties to validate their
implementation.

## Test suite for `massiv`

Internally `massiv-test` package contains all of the tests that are used on `massiv`. The
whole test suite has been extracted out to make the `massiv` package lighter as well as to
make the test functionality reusable, without impacting the dependency footprint of the
user that does not need the testing functionlity.

Because of this usecase, the major version of `massiv-test` is expected to increase with
almost every release of `massiv`.

## Doctests

Together with examples in haddock it is possible to describe various properties. Those
examples and properties can be tested with doctests, but such properties can not be tested
without QuickCheck generators readily available for import, for that reason `doctest` test
section of `massiv` also depends on `massiv-test`.

# More info

For more info on `massiv` and related libraries refer to
[README](https://github.com/lehins/massiv/blob/master/README.md) on github.


