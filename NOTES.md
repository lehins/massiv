# Benfits over Repa

* Directional parallel folds

* Delayed arrays are not indexable by default, preventing users from writing
  inefficient code.
* `I` and `W` are only instances of `Load` and not `Source` or `Manifest`, hence
  they can be computed, but not mistakenly converted back to delayed, thus
  loosing all the structure
