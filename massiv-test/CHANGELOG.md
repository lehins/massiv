# 1.1.0

* Rename `assertException` to `assertDeepException` and `assertExceptionIO` to
  `assertDeepExceptionIO` in order to match functions that were released in
  `QuickCheck-2.15`

# 1.0.0

* Support for massiv-1.0.0.0

# 0.1.7

* Add `propIO`

# 0.1.6

* Fix expectations for matrix multiplications. Empty arrays now always produce empty arrays.

# 0.1.5

* Add numeric tests
* Add floating point comparison with epsilon functions:
  * `epsilonExpect`
  * `epsilonFoldableExpect`
  * `epsilonMaybeEq`
  * `epsilonEq`
  * `epsilonEqDouble`
  * `epsilonEqFloat`


# 0.1.4

* Add `ArrDW`

# 0.1.0

* Initial release.
