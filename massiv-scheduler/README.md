# massiv-scheduler

Primary focus of this package is to provide work stealing scheduler for the array processing library
[massiv](https://www.stackage.org/package/massiv). But it can be used for any other project that can
benefit from parallelization of computation.

## QuickStart

A few examples in order to get up and running quickly.

### Schedule simple actions

Work scheduling that does some side effecty stuff and discards the results:

```haskell
interleaveFooBar :: IO ()
interleaveFooBar = do
  withScheduler_ (ParN 2) $ \ scheduler -> do
    putStrLn "Scheduling 1st job"
    scheduleWork_ scheduler (putStr "foo")
    putStrLn "Scheduling 2nd job"
    scheduleWork_ scheduler (putStr "bar")
    putStrLn "Awaiting for jobs to be executed:"
  putStrLn "\nDone"
```

In the example above two workers will be scheduled that will handle the only two jobs that have been
scheduled. Priniting iwith `putStr` is not thread safe, so you will notice that the output might get
interleaved:

```haskell
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
  withScheduler (ParOn [1..4]) $ \ scheduler -> do
    scheduleWork scheduler $ pure (10 + 1)
    scheduleWork scheduler $ pure (20 + 2)
    scheduleWork scheduler $ pure (30 + 3)
    scheduleWork scheduler $ pure (40 + 4)
    scheduleWork scheduler $ pure (50 + 5)
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
  withScheduler_ (ParN 5) $ \ scheduler -> do
    scheduleWork_ scheduler $ putStrLn $ repeat 'a'
    scheduleWork_ scheduler $ putStrLn $ repeat 'b'
    scheduleWork_ scheduler $ putStrLn $ repeat 'c'
    scheduleWork_ scheduler $ pure (4 `div` (0 :: Int))
    scheduleWork_ scheduler $ putStrLn $ repeat 'd'
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


### Nested jobs

Scheduling actions can themselves schedule actions indefinitely. That of course means that order of
results produced is no longer deterministic, which is to be expected.

```haskell
nestedJobs :: IO ()
nestedJobs = do
  withScheduler_ (ParN 5) $ \ scheduler -> do
    scheduleWork_ scheduler $ putStr $ replicate 10 'a'
    scheduleWork_ scheduler $ do
      putStr $ replicate 10 'b'
      scheduleWork_ scheduler $ do
        putStr $ replicate 10 'c'
        scheduleWork_ scheduler $ putStr $ replicate 10 'e'
      scheduleWork_ scheduler $ putStr $ replicate 10 'd'
    scheduleWork_ scheduler $ putStr $ replicate 10 'f'
  putStrLn "\nDone"
```

The order of how characters appear is important, since it actually shines a light on the actual
order in which jobs are being scheduled:

* `c`, `d` and `e` characters will always appear after `b`
* `e` will always appear after `c`

```haskell
λ> nestedJobs
abbafbafbafbafbafbafbafbafbaffcdcdcdcdcdcdcdcdcdcdeeeeeeeeee
Done
```

### Nested parallelism

Nothing really prevents you from having a scheduler within a scheduler. Of course, having multiple
schedulers at the same time seems like an unnecessary overhead, which it is, but if you do have a
use case for it, then is OK to go that route.

```haskell
nestedSchedulers :: IO ()
nestedSchedulers = do
  withScheduler_ (ParN 2) $ \ outerScheduler -> do
    scheduleWork_ outerScheduler $ putStr $ replicate 10 'a'
    scheduleWork_ outerScheduler $ do
      putStr $ replicate 10 'b'
      withScheduler_ (ParN 2) $ \ innerScheduler -> do
        scheduleWork_ innerScheduler $ do
          putStr $ replicate 10 'c'
          scheduleWork_ outerScheduler $ putStr $ replicate 10 'e'
        scheduleWork_ innerScheduler $ putStr $ replicate 10 'd'
    scheduleWork_ outerScheduler $ putStr $ replicate 10 'f'
  putStrLn "\nDone"
```

Note that inner scheduler's job schedules a job for the outer scheduler, which a bit crazy, but
totally safe.

```haskell
λ> nestedSchedulers
aabababababababababbffffffffffcccccccdcdcdcdddededededeeeeee
Done
```

### Single worker schedulers

If we only have one worker, than everyting becomes sequential and derterministic. Consider the saem
example from before, but with `Seq` computation strategy.

```haskell
nestedSequentialSchedulers :: IO ()
nestedSequentialSchedulers = do
  withScheduler_ Seq $ \ outerScheduler -> do
    scheduleWork_ outerScheduler $ putStr $ replicate 10 'a'
    scheduleWork_ outerScheduler $ do
      putStr $ replicate 10 'b'
      withScheduler_ Seq $ \ innerScheduler -> do
        scheduleWork_ innerScheduler $ do
          putStr $ replicate 10 'c'
          scheduleWork_ outerScheduler $ putStr $ replicate 10 'e'
        scheduleWork_ innerScheduler $ putStr $ replicate 10 'd'
    scheduleWork_ outerScheduler $ putStr $ replicate 10 'f'
  putStrLn "\nDone"
```

No more interleaving, everything is done in the same order each time the function is invoked.

```haskell
λ> nestedSchedulers
aaaaaaaaaabbbbbbbbbbccccccccccddddddddddffffffffffeeeeeeeeee
Done
```

## Avoiding deadlocks

Any sort of concurrency primitives such as mutual exclusion, semaphors, etc. can easily lead to
deadlocks, starvation and other common problems. Try to avoid them and be careful if you do end up
using them.

