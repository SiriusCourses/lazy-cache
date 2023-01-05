{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Cheops.Cache.CacheSpec where

import Control.Exception
import Data.IORef
import System.Clock.Seconds
import Data.Hashable
import Test.Hspec
import Test.HUnit.Base               ((@=?))

import qualified System.Cache                  as Cache
import qualified System.Cache.Impl.Ghc         as Ghc
import qualified System.Cache.Impl.MVar        as MVar
import System.Cache.Internal.Interface as Cache

spec :: Spec
spec = do
  describe "GHC"  $ inner (Ghc.new)
  describe "MVar" $ inner (MVar.new)

inner :: (forall a b . (Show a, Hashable a, Ord a) => Cache.Config -> IO (Cache.Handle a b)) -> Spec
inner new = describe "Cache service" $ do
  let configClock = Monotonic
  let configLongestAge = fromNanoSecs (100 * 1_000_000_000)
  it "cache store values" $ do
    ch <- new Cache.Config{..}
    ir <- newIORef True
    let
      f _ = do
        firstTime <- readIORef ir
        if firstTime then writeIORef ir False >> pure 0 else pure 1
    ((0 :: Int) @=?) =<< Cache.requestOr ch () f
    ((0 :: Int) @=?) =<< Cache.requestOr ch () f
  it "honour input" $ do
    ch <- new Cache.Config{..}
    let f x = pure x
    ((3 :: Int) @=?) =<< Cache.requestOr ch 3 f
    ((4 :: Int) @=?) =<< Cache.requestOr ch 4 f
    ((3 :: Int) @=?) =<< Cache.requestOr ch 3 f
  it "drops caches" $ do
    ch <- new Cache.Config{ configLongestAge = fromNanoSecs 0, ..}
    ir <- newIORef True
    let
      f _ = do
        firstTime <- readIORef ir
        if firstTime then writeIORef ir False >> pure 0 else pure 1
    ((0 :: Int) @=?)
      =<< Cache.requestOrInternal ch (fromNanoSecs (0)) () f
    ((1 :: Int) @=?)
      =<< Cache.requestOrInternal ch (fromNanoSecs (2 * 1_000_000_000)) () f
  it "handles exceptions" $ do
    ch <- new Cache.Config{..} :: IO (Cache.Handle Int Int)
    ((Left (ErrorCall "foo")) @=?) =<<
       either (\(ErrorCallWithLocation s _) -> Left (ErrorCall s)) Right
         <$> try (Cache.requestOr ch (3::Int) (\_ -> error "foo"))
    -- second call is still ok
    ((Left (ErrorCall "bar")) @=?) =<<
       either (\(ErrorCallWithLocation s _) -> Left (ErrorCall s)) Right
         <$> try (Cache.requestOr ch (3::Int) (\_ -> error "bar"))
    (Right 4 @=?) =<<
       either (\(ErrorCallWithLocation s _) -> Left (ErrorCall s)) Right
         <$> try (Cache.requestOr ch (3::Int) (\_ -> pure 4))
