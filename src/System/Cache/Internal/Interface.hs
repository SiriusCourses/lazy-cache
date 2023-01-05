-- | 
--
-- Generic interface for caching the 'IO' actions.
--
module System.Cache.Internal.Interface
  ( Handle(..)
  , Config(..)
  ) where

import System.Clock.Seconds

-- | @Handle a b@ is an internal Interface for running an actions, it wraps two methods:
--
--   * @requestOrInternal s a f@ — runs an action @f@ that takes a parameter @a@ and stores successful 
--        result for time no longer than @s@.
--   * @remove a@ — removes the result cached for input @a@.
--   * @getClockTime@ — finds current time.
--
-- Properties that should be supported by the implementation:
--  1. If the function return succesfully then the result should be cached for a time period,
--     All later calls should return the cached value.
--  2. In case of the concurrent actions an implementation should be the best effort to avoid
--     reduntant calls
data Handle a b = Handle
  { requestOrInternal :: Seconds -> a -> (a -> IO b) -> IO b
  , remove :: a -> IO ()
  , getClockTime :: IO Seconds
  }

-- | Configuration for 'Cache'
data Config = Config
  { configLongestAge :: Seconds
  , configClock :: Clock
  }

