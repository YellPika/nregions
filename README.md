nregions
========

Monadic regional resource management with support for threads.

Usage
-----

A regional computation is represented by the `RegionT` monad transformer.
Run a `RegionT` with the `runRegionT` function.

```
import Control.Monad.Trans.Region

myComputation :: RegionT t c IO ()
myComputation = do
  lift $ putStrLn "Hello!"
  ...
  return ()

main :: IO ()
main = runRegionT myComputation
```

The simple resource type available is the `Handle`, which allows you to specify "add reference" and "remove reference" functions. You can create more complex resources by encapsulating a `Handle`.

```
myComputation :: RegionT t c IO ()
myComputation = do
  handle <- newHandle (putStrLn "Adding Ref") (putStrLn "Removing Ref")
  ... -- Do something with handle
  return ()

main :: IO ()
main = runRegionT myComputation
```

Output:
```
Adding Ref
Removing Ref
```

The `scope` function creates a nested region.
Resources in the parent region are inaccessible, unless imported with `capture`.
Resources can be transferred to the parent region with `escape`.

```
myComputation = do
  handle <- newHandle ...
  doSomethingWith handle

  handle' <- scope $ do
    doSomethingWith handle -- compiler error

    handle' <- capture handle
    doSomethingWith handle

    -- return handle' would be a compiler error
    escape handle'

  doSomethingWith handle'
```

The `reset` function also creates a nested region.
However, resources in the parent region are automatically captured.
Resources created within the `reset` region cannot escape to the parent scope.
This function is ideal for threading.

```
myComputation = do
  handle1 <- newHandle ...
  handle2 <- newHandle ...

  scope $ do
    handle1' <- capture handle1

    reset forkIO $ do
      doSomethingWith handle1'
      doSomethingWith handle2 -- Compiler error

      capture handle2 -- Compiler error

      handle3 <- newHandle ...
      escape handle3 -- Compiler error
```

When threading is needed, the general idea is to `capture` the shared resources into their own `scope`, and then fork a thread with `reset`.
