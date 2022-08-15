# massiv-bench

In order to run all benchmarks with individual reports:

```
$ stack bench --no-run-benchmarks
$ for target in $(stack ide targets 2>&1 | grep ":bench:"); do stack bench $target --ba="--output 1.0.1-`echo $target | cut -d ':' -f3`.html"; done
```
