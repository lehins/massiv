#!/usr/bin/env bash

set -euo pipefail

cabal repl massiv --build-depends=QuickCheck --build-depends=random --build-depends=mwc-random --build-depends=splitmix --build-depends=mersenne-random-pure64 --with-compiler=doctest --repl-options='-w -Wdefault'
