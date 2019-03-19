# massiv-scheduler

Primary focus of this package is to provide work stealing scheduler for the array processing library
[massiv](https://github.com/lehins/massiv). But it can be used for any other project that can
benefit from parallelization of computation.

## QuickStart

A few examples in order to get up and running quickly.

### Schedule simple actions

Work scheduling that does some side effecty stuff and discards the results:

```haskell

interleaveFooBar :: IO ()
interleaveFooBar = do
  withScheduler_ (ParN 2) $ \ s -> do
    putStrLn "Scheduling 1st job"
    scheduleWork_ s (putStr "foo")
    putStrLn "Scheduling 2nd job"
    scheduleWork_ s (putStr "bar")
    putStrLn "Awaiting for jobs to be executed:"
  putStrLn "\nDone"
```

In the example above two workers will be scheduled that will handle the only two jobs that have been
scheduled. Priniting iwith `putStr` is not thread safe, so you will notice that the output might get
interleaved:

```
λ> interleaveFooBar
Scheduling 1st job
Scheduling 2nd job
Awaiting for jobs to be executed:
foboar
Done
```

Important to note that only when inner action supplied to the `withScheduler_` exits will the
scheduler start executing scheduled jobs.

### Keeping the results of computation

Another comon scenario is to schedule some jobs that produce useful results. In the example below
four works will be spawed off. Due to `ParOn` each of the workers will be pinned to a praticular
core.

```haskell
scheduleSums :: IO [Int]
scheduleSums =
  withScheduler (ParOn [1..4]) $ \ s -> do
    scheduleWork s $ pure (10 + 1)
    scheduleWork s $ pure (20 + 2)
    scheduleWork s $ pure (30 + 3)
    scheduleWork s $ pure (40 + 4)
    scheduleWork s $ pure (50 + 5)
```

Despite that the fact that sums are computed in parallel, the results of computation will appear in
the same order they've been scheduled:

```haskell
λ> scheduleSums
[11,22,33,44,55]
```

### Exceptions

Whenever any of the scheduled jobs result in an exception, all of the workers will be killed and the
exception will get re-thrown in the scheduling thread:

```haskell
infiniteJobs :: IO ()
infiniteJobs = do
  withScheduler_ (ParN 5) $ \ s -> do
    scheduleWork_ s $ putStrLn $ repeat 'a'
    scheduleWork_ s $ putStrLn $ repeat 'b'
    scheduleWork_ s $ putStrLn $ repeat 'c'
    scheduleWork_ s $ pure (4 `div` (0 :: Int))
    scheduleWork_ s $ putStrLn $ repeat 'd'
  putStrLn "\nDone"
```

Note, that if there was no exception, priniting would never stop.

```haskell
λ> infiniteJobs
aaaaaaaaabcdd*** Exception: divide by zero
```

A special case is when a thread is killed by an async exception. Whenever that happens that
exception will be rethrown in a scheduling thread wrapped in a custom `WorkerAsyncException`
exception. If for some reason you need to recover the original async exception you can use
`fromWorkerAsyncException`. See function documentation for an example.
