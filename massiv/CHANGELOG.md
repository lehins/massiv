# 0.3.5

* Fix and export `guardNumberOfElements`
* `Eq` instances for `IndexException` and `SizeException`

# 0.3.4

* Use the the new stateful workers feature of `scheduler-1.4.0`
* Addition of:
  * `randomArrayS`
  * `randomArrayWS`
  * `generateArrayWS`
  * `generateArrayLinearWS`
  * `mapWS`, `forWS`, `imapWS` and `iforWS`
  * and `splitLinearlyWithStatefulM_`

# 0.3.3

* Fix type signature for `createArray`.
* Support for new version of `scheduler`
* Addition of `randomArray`

# 0.3.2.1

* Fix `sqrtA` function: [#76](https://github.com/lehins/massiv/pull/76)

# 0.3.2

* Exported `withMArrayS`
* Switch to pure exception throwing for `read'`, `write'`, `modify'` and `swap'`. `MonadThrow`
  constraint prevented those functions to be used in `ST` monad.
* Addition of `quicksort`, `quicksortM_`, `unstablePartitionRegionM` and
  `unsafeUnstablePartitionRegionM`

# 0.3.1

* Addition of `rangeStepInclusive'`
* Addition of `flatten`
* `makeLoadArray` has been deprecated into `unsafeMakeLoadArray`.
* A new safe `makeLoadArrayS` has been aded.
* Fix `infix 4` for `(...)` and `(..:)` range functions, so they can be easily composed with
  numeric operations
* Addition of `imapSchedulerM_` and `iforSchedulerM_`

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
  computation strategy. For that reason these folds have been removed: `foldlOnP`, `ifoldlOnP`,
  `foldrOnP`, `ifoldrOnP`
* `fold` now is just like the one from `Data.Foldable` takes no arguments and requires elements to
  be a monoid
* `singleton` does not accept computation strategy any more and creates `Seq` array by default
* New function `empty`.
* `Ragged` functions are no longer exported, until the interface stabilizes and proper
  implementation of ragged arrays is in place.
* Partial functions `read'`, `write'` and `swap'` now live in IO and throw proper exceptions.
* `loadArray` is renamed to `loadArrayM` and there is a new separate function (not part of `Load`
  class) with the name `loadArray` that actually uses `loadArrayM`
* Moved `unsafeWithPtr` into `Data.Massiv.Array.Unsafe`
* Addition of:
  * `unsafeArrayToForeignPtr`,
  * `unsafeMArrayToForeignPtr`,
  * `unsafeArrayFromForeignPtr`,
  * `unsafeArrayFromForeignPtr0`,
  * `unsafeMArrayFromForeignPtr`,
  * `unsafeMArrayFromForeignPtr0`
* Addition of `castToByteString`, `castFromByteString`
* Addition of `makeUnsafeStencil`
* `Window` now has an `windowUnrollIx2` field.
* Addition of `insertWindow` and `dropWindow`

# 0.2.8.1

* Fix `sqrtA` function. Backport of [#76](https://github.com/lehins/massiv/pull/76)

# 0.2.8

* Fixed a problem where convolution stencil size was not inverted, causing out of bounds memory
  read: [#72](https://github.com/lehins/massiv/issues/72)
* Fixed an issue with windowed array where a stencil size is smaller than the array it is applied to
* Fixed incorrect cross-correlation stencil construction

# 0.2.7

* Fixed a serious performance regression in `Stencil`'s `Functor` instance, which was introduced in
  version `0.2.3`
* Added type and pattern synonyms `Sz` for future compatibility with version `0.3`. Could be useful
  for migration.

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
* Addition of cute synonyms: `(...)` and `(..:)`

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
