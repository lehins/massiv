# 0.1.7

* `Show` instance for `DW` arrays.
* Create new `Data.Massiv.Array.Stencil.Unsafe` module and move `forStencilUnsafe` into it.

# 0.1.6

* `Semigroup` and `Monoid` instance for `Value`.
* Addition of `forStencilUnsafe`.
* Fix `minimum` behaving as `maximum`.
* Addition of `foldSemi`.

# 0.1.5

* Fix inverted stencil index calculation [#12](https://github.com/lehins/massiv/issues/12)
* Add support for cross-correlation.

# 0.1.4

* Addition of Monoidal folding `foldMono`.
* Expose `liftArray2`.

# 0.1.3

* Addition of `withPtr` and `unsafeWithPtr` for Storable arrays
* Addition of `computeInto`.
* Exposed `makeWindowedArray`.

# 0.1.2

* Support for GHC-8.4 - instance of `Comp` for `Semigroup`
* Brought back support for GHC-7.10

# 0.1.1

* Addition of experimental `mapM`, `imapM`, `forM`, `iforM`, `generateM` and `generateLinearM`
  functions. Fixes #5
* Addition of `Ord` instances for some array representations.

# 0.1.0

* Initial Release
