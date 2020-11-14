{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Cache
  ( Handle(..)
  , Config(..)
  , new
  , requestOr
  , mkConfig
  ) where

import Data.Hashable
import Data.Thyme.Clock
import Data.Thyme.Clock.POSIX

import System.Cache.Internal.Interface
import qualified System.Cache.Impl.Ghc as Ghc
import qualified System.Cache.Impl.MVar as MVar
import System.Environment
import System.IO.Unsafe

mkConfig :: Int -> Config
mkConfig n = Config { configLongestAge = fromSeconds n }

-- | Set default cache implementation.
new :: (Show a, Hashable a, Ord a) => Config -> IO (Handle a b)
{-# NOINLINE new #-}
new = unsafePerformIO $ do
  lookupEnv "GHC_CACHE_IMPL" >>= \case
    Just "MVAR" -> pure $ MVar.new
    _ -> pure $ Ghc.new


-- | Perform a request substituting current time.
requestOr :: Handle a b -> a -> (a -> IO b) -> IO b
requestOr Handle {..} k f = do
  tm <- getPOSIXTime
  requestOrInternal tm k f

