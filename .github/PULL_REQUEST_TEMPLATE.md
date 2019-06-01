Please include this checklist whenever changes are introduced to either `massiv` or `massiv-io` packages:

* [ ] Bump up the version in cabal file
* [ ] Any changes that could be relevant to users have been recorded in the `CHANGELOG.md`
* [ ] The documentation has been updated, if necessary.
* [ ] Property tests or at least some unit test cases been added for all new functionality.
* [ ] Link to any issues that might be related to this Pull Request.

Here is also a [great guide from Michael Snoyman](https://www.snoyman.com/blog/2017/06/how-to-send-me-a-pull-request) you can follow when submitting a PR that touches those packages.

Formatting recommendations:

* I personally use [hindent](https://www.stackage.org/package/hindent) on almost functions in the codebase, highly recommend it.
* [stylish-haskell](https://www.stackage.org/package/stylish-haskell) for organazing imports.
* If not using above formatting tools, please, just try to follow the general style present in the codebase: eg. 2 space indentation, no tabs, etc.
* Can't stand trailing whitespaces. If using `emacs`, there is `M-x delete-trailing-whitespace`.
