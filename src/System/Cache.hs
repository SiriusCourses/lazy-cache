{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- General interface for the cache. This module is intended to be used
-- in th user code.
--
-- __TLDR__
-- 
-- @
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE NumericUnderscores #-}
-- {-# LANGUAGE BlockArguments #-}
-- import Control.Concurrent
-- 
-- import "System.Cache" qualified as Cache
-- import "System.Cache.Impl.Ghc" qualified as Cache.Ghc
-- import System.Clock.Seconds
-- 
-- main :: IO ()
-- main = do
--    -- we create a new cache handle that acts as a storage for
--    -- cached values
--    cache <- Cache.Ghc.'System.Cache.Impl.Ghc.new' do
--      Cache.'System.Cache.mkConfig' 60 'System.Clock.Seconds.MonotonicCoarse'
--    -- we create a cached version of computation
--    -- in order to hide implementation
--    let cachedTimeout = Cache.'System.Cache.mkCached' @cache@ \i -> do
--          threadDelay $ i * 1_000_000
--          pure i
--    -- We use our cached function
--    print (cachedTimeout 1)
-- @
--
module System.Cache
  ( -- * API
    -- ** Create
    -- $api1
    Handle
    -- $api2
  , Config
  , mkConfig
    -- ** Use
    -- $api3
  , requestOr
    -- $api4
  , cacheIO
  -- * Helpers
  -- $helpers
  , new
  , mkCached
  ) where

import Data.Functor
import Data.Hashable
import System.Clock.Seconds

import System.Cache.Internal.Interface
import qualified System.Cache.Impl.Ghc as Ghc
import qualified System.Cache.Impl.MVar as MVar
import System.Environment
import System.IO.Unsafe

-- $api1
--
-- In order to use this library you first need to explicitly create a storage
-- for the values. This storage is implemented by the abstract type 'Handle' that
-- provides only a public function interface but hides an actual implementation.
-- You can find full description of the 'Handle' in the "System.Cache.Internal.Interface"
-- module.

-- $api2
--
-- In order to create a 'Handle' you'll need to call appropiate function from the
-- @System.Cache.Impl.\*.new@ module. This way you can chose an actual implementation.
-- Alternatively you can use 'System.Cache.new' function that will make a choise for you, or
-- create your own implementation.
--
-- Each of those functions take a 'Config' as a parameter

-- $api3
--
-- Once the storage is created you can use it for caching values via 'requestOr' method.

-- $api4
--
-- The API of the method is not safe enough because you can for example ignore input
-- value in the funciton, or pass different function in different invocations with the 
-- same storage. One the one hand we do not want to prevent such usages as may be done
-- for a purpose, but in order to provide additional safety we provide more safe methods:

-- | Helper to create a config.
mkConfig
  :: Int -- ^ Max time that the value can be cached (in seconds).
  -> Clock -- ^ Type of the clock that the cache will use.
  -> Config
mkConfig n clock = Config
  { configLongestAge = fromNanoSecs (fromIntegral n * 1_000_000_000)
  , configClock = clock
  }


-- | Perform a request.
requestOr
  :: Handle a b
  -> a -- ^ Input parameters
  -> (a -> IO b) -- ^ Function to cache.
  -> IO b -- ^ Result.
requestOr Handle {..} k f = do
  tm <- getClockTime
  requestOrInternal tm k f

-- | Wraps an IO action and returns a cached version of that method.
cacheIO
  :: Handle a b  -- ^ Values storage
  -> (a -> IO b) -- ^ Action to cache
  -> (a -> IO b) -- ^ A version that caches the result
cacheIO handle f = \k -> requestOr handle k f

-- | Set default cache implementation. This method perfers to use 
-- "System.Cache.Impl.Ghc" implementation unless @GHC_CACHE_IMPL@ environment value
-- has value @MVAR@ in this case "System.Cache.Impl.MVar" is used.
--
-- This method is useful as a default one because it prefers a faster and stabler
-- implementation, but in case of emergency it allows to switch to the conservative
-- implementation without program recompilation.
--
-- __N.B.__ this methos uses 'unsafePerformIO'.
new
  :: (Show a, Hashable a, Ord a)
  => Config
  -> IO (Handle a b)
{-# NOINLINE new #-}
new = unsafePerformIO $ do
  lookupEnv "GHC_CACHE_IMPL" >>= \case
    Just "MVAR" -> pure $ MVar.new
    _ -> pure $ Ghc.new

-- | Wrapper for creatio cached values. This wrapper does not expose raw 'Handle',
-- so it will not be possible to unexpectedly reuse between different methods.
--
-- @
-- cachedIO <- 'mkCached' 'System.Cache.Impl.Ghc.new' config io
-- @
mkCached
  :: (Config -> IO (Handle a b))  -- ^ Create handle function
  -> Config                       -- ^ Config for caching
  -> (a -> IO b)                  -- ^ Action to cache
  -> IO (a -> IO b)               -- ^ Version that caches the result
mkCached mk config f =
  mk config <&> \handle -> cacheIO handle f
