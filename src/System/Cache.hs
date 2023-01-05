{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- General interface for the cache. This module is intended to be used
-- in th user code.
module System.Cache
  ( Handle(..)
  , Config(..)
  , new
  , requestOr
  , mkConfig
  ) where

import Data.Hashable
import System.Clock.Seconds

import System.Cache.Internal.Interface
import qualified System.Cache.Impl.Ghc as Ghc
import qualified System.Cache.Impl.MVar as MVar
import System.Environment
import System.IO.Unsafe

-- | Wrapper to generate a config.
mkConfig :: Int -> Clock -> Config
mkConfig n clock = Config
  { configLongestAge = fromNanoSecs (fromIntegral n * 1_000_000_000)
  , configClock = clock
  }

-- | Set default cache implementation.
new :: (Show a, Hashable a, Ord a) => Config -> IO (Handle a b)
{-# NOINLINE new #-}
new = unsafePerformIO $ do
  lookupEnv "GHC_CACHE_IMPL" >>= \case
    Just "MVAR" -> pure $ MVar.new
    _ -> pure $ Ghc.new


-- | Perform a request.
requestOr
  :: Handle a b
  -> a -- ^ Input parameters
  -> (a -> IO b) -- ^ Function to cache.
  -> IO b -- ^ Result.
requestOr Handle {..} k f = do
  tm <- getClockTime
  requestOrInternal tm k f

