#!/usr/bin/env bash

set -euo pipefail

if [[ $# -gt 0 ]]; then
  case "$1" in
    --changes)
      # Run fourmolu on changes compared to `master`.
      git diff --diff-filter=MA --name-only origin/master HEAD -- '*.hs'
      ;;
    *)
      echo "Invalid option: $1" >&2
      exit 1
      ;;
  esac
else
  git ls-files -- '*.hs'
fi \
  | { grep -v Setup.hs || true; } \
  | xargs -r fourmolu -m inplace

git diff --exit-code
