# 0.3.0

* Class hierarchy an associated methods:
  * `getComp` moved from `Construct` to `Load`
  * `Size` class lost array value parameter `e`. `unsafeResize` and `unsafeExtract` became their own
    classes
* New classes:
  * `Resize` with `unsafeResize` from old `Size`, except with `array` type parameter for
    applicability to mutable `MArray`s
  * `Extract` with `unsafeExtract` from old `Size`
  * `StrideLoad`, child of `Load`
* `ifoldlIO` and related no longer take list of capabilities, but instead respect the inner
  computation strategy. FOr that reason these folds have been removed: `foldlOnP`, `ifoldlOnP`,
  `foldrOnP`, `ifoldrOnP`
* `fold` now is just like the one from `Data.Foldable` takes no arguments and requires elements to
  be a monoid
* `singleton` does not accept computation strategy any more and creates `Seq` array by default
* New function `empty`.
* Partial functions `read'`, `write'` and `swap'` now live in IO and throw proper exceptions.
* `loadArray` is renamed to `loadArrayM` and there is a new separate function (not part of `Load`
  class) with the name `loadArray` that actually uses `loadArrayM`

# 0.2.6

* Add `expand*` family of functions.
* Long awaited `makeArrayM`/`makeArrayA` and `mapM`/`forM`/`imapM`/`iforM`/`traverseA`/`itraverseA`
  alnog with corresponding functions allowing for supplying representation.
* Deprecate `mapP` and `mapP_` in favor of `mapIO` and `mapIO_`, while making latter respect the
  `Comp`.
* Addition of a whole collection of mutable operators:
  * `mapIO`/`mapIO_`/`imapIO`/`imapIO_`/`forIO`/`forIO_`/`iforIO`/`iforIO_`
  * `createArray`/`createArrayST`/`createArrayST_`
  * `generateArray`/`generateArrayIO`
  * `unfoldlPrim`/`unfoldlPrim_`
  * `makeArrayA`, `makeArrayAR`

# 0.2.5

* Fix for `insertDimension` [#62](https://github.com/lehins/massiv/pull/62)

# 0.2.4.1

* Fix a bug in `zip` functions, where resulting array size would not take into account the size of
  one of the input arrays.


# 0.2.4

* Addition of inner folds: `ifoldlInner`, `foldlInner`, `ifoldrInner` and `foldrInner`
* Addition of functions that can fold over any dimension (`foldlWithin`, `foldlWithin'`, etc.)
* Addition of `ifoldMono` and `ifoldSemi`, thus fixing:
  [#54](https://github.com/lehins/massiv/issues/54)
* Improvement over manipulating index dimensions with addition of type level `Dimension n` data type
  and functions like `getDimension`, `dropDimension`.
* Addition of `insertDim` and type level `insertDimension` as well as `pullOutDim` and
  `pullOutDimension`
* Add partial `extractFromTo'`

# 0.2.3

* Addition of `Profunctor` functions for `Stencil`: `lmapStencil`, `rmapStencil` and `bimapStencil`
* Addition of integration approximation: `Data.Massiv.Array.Numeric.Integral`
* Removed overlapping instances for `DW` in favor of concrete instances.
* Relaxed contraint restrictions on matrix multiplication `(|*|)` and slighly improved performance
  with rewrite rules to avoid double transform.

# 0.2.2

* Addition of `withMArray`, `withMArrayST`.
* Improved preformance of matrix multiplication

# 0.2.1

* Addition of `Stride` and related functions `computeWithStride` and `computeWithStrideAs`.
* Addition of `Window`
* Addition of `loadArray` adn `loadArrayWithStride` with default implementations that will become
  new loading functions in a subsequent release. `loadArray` will replace `loadS` and `loadP`, which
  will be deprecated in the next release and removed in the next major release. Some of this is
  discussed in [#41](https://github.com/lehins/massiv/issues/41)
* Addition of various conversion functions:

  * `fromByteString`, `toByteString` and `toBuilder`
  * `unwrapArray`, `evalArray`, `unwrapMutableArray`, `evalMutableArray`
  * `unwrapNormalFormArray`, `evalNormalFormArray`, `unwrapNormalFormMutableArray`,
    `evalNormalFormMutableArray`

* Fix: `Eq` instance for `Array M ix e`

# 0.2.0

* Fixed type signatures for `convertAs` and `convertProxy`
* Added type constructors for `DW` and `DI`
* `Show` instance for `DW` arrays.
* Addition of `unsafeBackpermuteDW`.
* Breaking changes:
  * Create new `Data.Massiv.Array.Stencil.Unsafe` module and move `forStencilUnsafe` into it.
  * Rename of rank -> dimensions #25
    * Removal `Eq` and `Ord` instances for `Value` #19
    * Move border resolution to `mapStencil` from `makeStencil`.
  * Updated iterators `iterM`, `iterM_`, etc. to have a separate step per dimension.

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
