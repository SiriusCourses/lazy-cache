# Library for caching values.

The library allows to cache result of an asynchronous computation for
no longer than a given period of time.

# How to use

In order to use the package you should add the package to the dependency
section of the cabal file of your package.

In order to use the cache you should create a 'Handle'

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BlockArguments #-}
import Control.Concurrent

import System.Cache qualified as Cache
import System.Cache.Impl.Ghc qualified as Cache.Ghc
import System.Clock.Seconds

main :: IO ()
main = do
   cache <- Cache.Ghc.new do
     Cache.mkConfig 60 Monotonic
   -- we create a cached version of computation
   -- in order to hide implementation
   let cachedTimeout input = Cache.cacheIO cache \i -> do
         threadDelay $ i * 1_000_000
         pure i
   -- We use our cached function
   print (cachedTimeout 1)
   print (cachedTimeout 1)
   print (cachedTimeout 2)
```

```
sh-3.2$ runhaskell local.hs | ts -S
00:00:03 2
00:00:03 2
00:00:06 3
```

When running the program first we wait for 2 seconds, the next call returns immediately.
The third one is long as it uses a new input value.


# Developing the package

```
cabal configure
cabal build
```
