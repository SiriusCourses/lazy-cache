{-# LANGUAGE RecursiveDo #-}
module System.Cache.Impl.NoCache
  ( new
  ) where

import System.Cache.Internal.Interface

new :: IO (Handle a b)
new = do
  pure $ Handle
    { requestOrInternal = \_ k f -> f k
    , remove = \_ -> pure ()
    }
