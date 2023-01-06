{-# LANGUAGE RecursiveDo #-}
-- | Implementation that provides no caching at all.
-- Can be useful in order to disable caching without changing
-- the code at the use sites.
module System.Cache.Impl.NoCache
  ( new
  ) where

import System.Cache.Internal.Interface
import System.Clock.Seconds

-- | Creates an implementation that does not cache anything.
-- All actions runs as-is without locking.
new :: Config -> IO (Handle a b)
new Config{..} = do
  pure $ Handle
    { requestOrInternal = \_ k f -> f k
    , remove = \_ -> pure ()
    , getClockTime = getTime configClock
    }
