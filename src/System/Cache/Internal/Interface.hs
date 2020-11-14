{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Cache.Internal.Interface
  ( Handle(..)
  , Config(..)
  ) where

import           Data.Thyme.Clock
import           Data.Thyme.Clock.POSIX

-- | Cache handle.
data Handle a b = Handle
  { requestOrInternal :: POSIXTime -> a -> (a -> IO b) -> IO b
  , remove :: a -> IO ()
  }

-- | Configuration for 'Cache'
data Config = Config
  { configLongestAge :: NominalDiffTime }

