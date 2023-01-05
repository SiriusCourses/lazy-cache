{-# LANGUAGE RecursiveDo #-}
module System.Cache.Impl.NoCache
  ( new
  ) where

import System.Cache.Internal.Interface
import System.Clock.Seconds

new :: Config -> IO (Handle a b)
new Config{..} = do
  pure $ Handle
    { requestOrInternal = \_ k f -> f k
    , remove = \_ -> pure ()
    , getClockTime = getTime configClock
    }
