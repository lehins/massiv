# 0.5.9

* Add `mallocCompute`, `mallocCopy` and `unsafeMallocMArray`
* Fix `><.` and `.><` on empty matrices.

# 0.5.8

* Improve loading of push arrays by adding `loadArrayWithSetM` and deprecating `defaultElement`.

# 0.5.7

* Improve performance of `><.` and `><!` while making their constraints a bit more relaxed.
* Expose `unsafeLoadIntoM` and `unsafeLoadIntoS`
* Expose `eqArrays` and `compareArrays`
* Add `multiplyMatrixByVector` and `multiplyVectorByMatrix`

# 0.5.6

* Fix `(-.)` (it was incorrectly implemented as a flip of `(.-)`
* Addition of numeric functions:
  * Partial: `!+!`, `!-!`, `!*!`, `!/!`
  * Reciprocal division `/.`
  * More efficient matrix-matrix multiplication: `.><.` and `!><!` (also helpers
    `multiplyMatrices` and `multiplyMatricesTransposed`)
  * More efficient matrix-vector multiplication: `.><` and `!><`
  * New vector-matrix multiplication: `><.` and `><!`
  * Dot product `dotM` and `!.!`
  * Norm `normL2`
* Deprecated `|*|` and `#>`

# 0.5.5

* Add `takeWhile`, `dropWhile` and `findIndex`
* Improve performance of `any`, `and`, `or`, `all`
* Add `elem`

# 0.5.4

* Addition of `unsafeTransformStencil`
* Add `zip4`, `unzip4`, `zipWith4`  and `izipWith4`
* Make `Resize` a superclass of `Source`
* Addition of `outerSlices`, `innerSlices`, `withinSlices` and `withinSlicesM`
* Addition of `stackSlicesM`, `stackOuterSlicesM` and `stackInnerSlicesM`
* Addition of `computeP`
* Fix perfomrmance issue of folding functions applied to arrays with `Seq` computation
  strategy.

# 0.5.3

* Fix `tanA` and `tanhA`. [#96](https://github.com/lehins/massiv/pull/96)
* Relax argument of `snoc` and `cons` constraint to `Load` vectors
* Improve `unsnocM` and `unconsM` by switching to `unsafeLinearSlice`, instead of delaying
  the array.
* Fix parallelization for windowed array when computed with stride
* Fix massiv doctests not being able to find massiv.h under NixOS

# 0.5.2

* Addition of `lowerTriangular` and `upperTriangular`
* Relax `identityMatrix` type to return an array of any `Num` type, not just `Int`.
* Addition of `unsafeMakeLoadArrayAdjusted`
* Add matrix-vector product (`(#>)`)
* Addition of `siterate`

# 0.5.1

* Fix `sfromListN` accepting a plain `Int` instead of `Sz1`, as well as switch to upper bound.
* Fix order of argumetns in `iforM`
* Restrict `szip*`, `szipWith*` and `sizipWith*` functions to flat vectors.
* Addition of `unsafeSUnfoldrN`, `unsafeSUnfoldrNM` and `unsafeSFromListN`
* Fix `sunfoldrN`, `sunfoldrNM` and `sfromListN` to not trust the supplied size.
* Move `isEmpty` into `Load` class
* Add `isNotEmpty`

# 0.5.0

* Remove `Show` instance from `Value`.
* Addition of `unsafeCreateArray`, `unsafeCreateArray_` and `unsafeCreateArrayS`
* Remove `Comp` argument from functions that ignore it and set it to `Seq`:
  * `createArrayS_`, `createArrayS`, `createArrayST_`, `createArrayST`
  * `unfoldrPrimM_`, `iunfoldrPrimM_`, `unfoldrPrimM`, `iunfoldrPrimM`
  * `unfoldlPrimM_`, `iunfoldlPrimM_`, `unfoldlPrimM`, `iunfoldlPrimM`
* Addition of `fromStorableVector` and `fromStorableMVector`
* Modify `toMutableByteArray` to produce a copy if dealing with slice.
* Addition of `toByteArrayM`, `toMutableByteArrayM`
* Change `replicate` to produce delayed load array `DL`
* Export unsafe stencil functions from `Data.Array.Massiv.Unsafe`, rather than from
  `Data.Massiv.Array.Stencil.Unsafe`.
* Implement `unsafeMapStencil` and deprecate `mapStencilUnsafe` and `forStencilUnsafe`
* Addition of `castToBuilder`
* Addition of conversion functions:
  * `unwrapNormalForm` and `evalNormalForm`
  * `toBoxedVector`, `toBoxedMVector`, `evalBoxedVector` and `evalBoxedMVector`
  * `unwrapByteArray` and `unwrapMutableByteArray`
  * `toPrimitiveVector`, `toPrimitiveMVector`, `fromPrimitiveVector` and
  `fromPrimitiveMVector`
  * `toStorableVector`, `toStorableMVector`, `fromStorableVector` and `fromStorableMVector`
  * `fromUnboxedVector` and `fromUnboxedMVector`
  * `unsafeBoxedArray`, `unsafeNormalBoxedArray`, `unsafeFromBoxedVector`
* Removed deprecated `traverseAR`, `itraverseAR`, `traversePrimR` and `itraversePrimR`
* Removed: `imapMR`, `imapMR`, `iforMR`, and `iforMR`
* Renamed:
  * `withMArray` to `withMArray_`,
  * `withMArrayS` to `withMArrayS_` and
  * `withMArrayST` to `withMArrayST_`
* Added versions that keep the artifact of mutable action: `withMArray`, `withMArrayS`,
  `withMArrayST`.

# 0.4.5

* Addition of `computeIO` and `computePrimM`
* Addition of `makeArrayLinearA`
* Addition of `traverseS`
* Fix regression in performance introduced in `massiv-0.4.0`

# 0.4.4

* Addition of `appendOuterM` and `concatOuterM`
* Addition of `zoom`
* Addition of `write_`, `modify_` and `swap_`

# 0.4.3

* Addition of `catMaybesS` and `tally`

# 0.4.3

* Addition of `applyStencil` and `Padding` with helper functions `noPadding` and `samePadding`.
* Addition of `foldlStencil`, `foldrStencil` and monoidal `foldStencil`.
* Addition of common generic stencils: `sumStencil`, `productStencil`, `avgStencil`,
  `maxStencil`, `minStencil` and `idStencil`.
* Addition of `mapStencilUnsafe` for the brave.
* Improve compile time error reporting for invalid dimensions.
* Fix incorrect loading of `DW` arrays of dimension higher than 3
* Addition of `foldOuterSlice`, `ifoldOuterSlice`, `foldInnerSlice` and
  `ifoldInnerSlice`. Fix for [#56](https://github.com/lehins/massiv/issues/56)

# 0.4.2

* Fix loading empty `DS` stream arrays of unknown size. Fix for [#83](https://github.com/lehins/massiv/issues/83).

# 0.4.1

* Introduction of `Stream` and `DS` representation:
  * `filterS`, `filterM`, `ifilterS`, `ifilterM`
  * `mapMaybeS`, `mapMaybeM`, `imapMaybeS`, `imapMaybeM`
  * `unfoldr`, `unfoldrN`
  * `takeS` and `dropS`
* Deprecated `traverseAR`, `itraverseAR`, `traversePrimR`, `itraversePrimR` (not feasible
  to keep duplicate functions just for representation, `TypeApplications` or
  `ScopedVariables` should be used instead.)
* Fix performance issue with copying of unboxed arrays and initialization of storable array.
* Addition of `unsafeLoadIntoS`, `unsafeLoadInto` and `maxSize`
* Addition of `reverse`, `reverse'` and `reverseM`
* Addition of `modifyDimension`, `modifyDimM`, and `modifyDim'`

# 0.4.0

* Made `Construct` a super class of `Mutable`
* Reimplement a safe version of `makeLoadArray`, that is parallelizable.
* Switch from `EltRepr r ix` to much simpler `R r`
* Remove `Construct` instance for `M` representation.
* `unsafeLinearSet` - length argument now accepts `Sz1` instead of an `Int`
* Renamed:
  * `forPrimM_` -> `forPrimM`
  * `iforPrimM_` -> `iforPrimM`
  * `iforLinearPrimM_` -> `iforLinearPrimM`
* Introduced new functions that do not mutate the original array: `forPrimM_`,
  `iforPrimM_` and `iforLinearPrimM_`
* Addition of `readM`, `writeM`, `modifyM`, `swapM`, `modifyM_`, `swapM_`
* Add an orphan instance of `MonadThrow` for `ST` monad for older versions of
  `exceptions`. See [ekmett/exceptions#72](https://github.com/ekmett/exceptions/pull/72)
* Deprecation of `read'`, `write'` `modify'` and `swap'`
* Make `modify` accept a monadic action, rather than a pure function. Also now it returns
  the old element.
* Make `swap` return the swapped elements.
* Addition of `unsafeLinearSwap` and `unsafeSwap`
* Expose `unsafeLinearModify` and `unsafeModify`
* Expose `Data.Massiv.Core.List`
* Expose `indexWith`, so macro `INDEX_CHECK` from `massiv.h` could be used outside massiv.
* Addition of `liftSz`
* Fixed `expand*` functions by making them accept `Sz1` instead of an `Int`
* Addition of `expandWithinM`
* Bunch of minor fixes to `Show` instances
* Extracted test-suite into it's own package.
* Stop accepting computation strategy for all functions that can be performed sequentially only:
  * `iterateN`
  * `iiterateN`
  * `unfoldrS_`
  * `iunfoldrS_`
  * `unfoldlS_`
  * `iunfoldlS_`
  * `makeArrayA`
  * `makeArrayAR`
  * `generateArrayLinearS`
  * `generateArrayS`
* Redefined most of the numeric operators with `Numeric` and `NumericFloat`. Will be
  required for SIMD operations.

# 0.3.6

* Addition of `unsafeArrayLinearCopy`, `unsafeLinearCopy`, `unsafeLinearShrink`, `unsafeLinearGrow`
* Implementation of `iterateUntil` and `iterateUntilM`
* `identityMatrix` - generation of identity matrix

# 0.3.5

* Fix and export `guardNumberOfElements`
* `Eq` instances for `IndexException` and `SizeException`
* Fix `upsample` implementation and improve its performance.
* Addition of `deleteRegionM`, `deleteRowsM` and `deleteColumnsM`

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
* A new safe `makeLoadArrayS` has been added.
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
* Relaxed contraint restrictions on matrix multiplication `(|*|)` and slightly improved performance
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
