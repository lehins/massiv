# 0.2.1

* Addition of instances for `GIF` sequences in `RGB` with `Alpha` and instanced with
  `GifDisposalMethod`

# 0.2.0

* Switch to `MonadIO`
* Write files with durability and atomicity guarantees
* Switch to `encodeM` and `decodeM`, as well as corresponding `encodeImageM` and `decodeImageM`
* Addition of `decodeWithMetadataM`
* Addition of `ConvertError`, `DecodeError` and `EncodeError`.
* Got rid of `ReadOptions`
* Switch to `Color` package for pixels and color space coversion

# 0.1.9

* Fix `HDR` decoding, i.e. `.hdr` and `.pic` file reading.

# 0.1.8

* Fix reading images that have more than 8bit per channel:
  [#85](https://github.com/lehins/massiv/issues/85)

# 0.1.7

* Fix compatibility with `JuicyPixels >= 3.3.0`
* Add `Traversable` instances for all `Pixel` types.
* Derive a few more instances for `X` and `Y` pixels.
* Drop dependency on `data-default` in favor of `data-default-class`

# 0.1.6

* Made it compatible with new `massiv >= 0.3` as well as the old ones.

# 0.1.5

* All decoded images will be read in sequentially, but will have default computation set to `Par`.

# 0.1.4

* Fixed wrongful export of `Bit` constructor.
* Added export of `fromDynamicImage` and `fromAnyDynamicImage`

# 0.1.3

* Fixed #22 - Invalid guard against image size
* Made sure format is inferred from all supported file extensions for auto decoding.

# 0.1.2

* Exposed `Elevator` internal functions.
* Deprecate ColorSpace specific functions (`liftPx`, `foldlPx`, etc.) in favor of Functor,
  Applicative and Foldable.

# 0.1.1

* Addition of `Ord` instances to Pixels.

# 0.1.0

* Initial Release
