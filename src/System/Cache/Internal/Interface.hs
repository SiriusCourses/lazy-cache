-- | 
--
-- Internal interface for the Cache handle. In general you are not interested in this
-- module and should use public API from the "System.Cache" instead, unless:
--
--   1. you need to implement your own handle,
--   2. you need to use internal functions like 'remove',
--   3. you need to provide your own 'getClockTime' implementation 
--
--
module System.Cache.Internal.Interface
  ( Handle(..)
  , Config(..)
  ) where

import System.Clock.Seconds

-- | The public interface for the cache values storage.
--
-- A storage is expected to provide the following properties:
--
--  1. If the function return succesfully then the result should be cached for a time period,
--     All later calls should return the cached value.
--  2. In case of the concurrent actions an implementation should be the best effort to avoid
--     reduntant calls
data Handle a b = Handle
  { requestOrInternal :: Seconds -> a -> (a -> IO b) -> IO b
      -- ^ runs an action @f@ that takes a parameter @a@ and stores successful 
      --   result for time no longer than @s@.
  , remove :: a -> IO ()
      -- ^ removes the result cached for input @a@.
  , getClockTime :: IO Seconds
      -- ^ finds current time.
  }

-- | Configuration for 'Cache'
--
-- See "System.Cache.Internal.Interface" for all details on the fields.
data Config = Config
  { configLongestAge :: Seconds
    -- ^ The time that the value can be kept in cache.
  , configClock :: Clock
    -- ^ Type of the clock that is used by cache, by changing this parameter you can
    -- optimize your usage for the different use cases, see "System.Clock.Seconds"
    -- documentation for explanation of the choises.
  }

