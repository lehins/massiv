Please include this checklist whenever changes are introduced to `massiv` package:

* [ ] Bump up the version in cabal file, when applicable.
* [ ] Any changes that could be relevant to users have been recorded in the `CHANGELOG.md`
* [ ] The documentation has been updated, if necessary.
* [ ] Property tests or at least some unit test cases been added for all new functionality.
* [ ] Linked issues that might be related to this Pull Request.

Formatting requirement:

* This repository is formatted with [fourmolu](https://github.com/fourmolu/fourmolu) and the [`fourmolu.yaml`](https://github.com/lehins/massiv/blob/master/fourmolu.yaml) config is included at the top level. Run `./scripts/fourmolize.sh` in order to make sure you changes adhere to formatting, since it is required for CI to pass.

Here is also a [great guide from Michael Snoyman](https://www.snoyman.com/blog/2017/06/how-to-send-me-a-pull-request) you can follow when submitting a PR that touches packages in this repo.

