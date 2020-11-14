{-# LANGUAGE RankNTypes #-}
module Cheops.Cache.CacheSpec where

import           Control.Exception
import qualified System.Cache                  as Cache
import qualified System.Cache.Impl.Ghc         as Ghc
import qualified System.Cache.Impl.MVar        as MVar
import           Data.IORef
import qualified Data.Thyme                    as Thyme
import Data.Hashable
import           Test.Hspec
import           Test.HUnit.Base               ((@=?))

spec :: Spec
spec = do
  describe "GHC"  $ inner (Ghc.new)
  describe "MVar" $ inner (MVar.new)

inner :: (forall a b . (Show a, Hashable a, Ord a) => Cache.Config -> IO (Cache.Handle a b)) -> Spec
inner new = describe "Cache service" $ do
  let d = Thyme.fromSeconds (100 :: Int)
  it "cache store values" $ do
    ch <- new (Cache.Config d)
    ir <- newIORef True
    let
      f _ = do
        firstTime <- readIORef ir
        if firstTime then writeIORef ir False >> pure 0 else pure 1
    ((0 :: Int) @=?) =<< Cache.requestOr ch () f
    ((0 :: Int) @=?) =<< Cache.requestOr ch () f
  it "honour input" $ do
    ch <- new (Cache.Config d)
    let f x = pure x
    ((3 :: Int) @=?) =<< Cache.requestOr ch 3 f
    ((4 :: Int) @=?) =<< Cache.requestOr ch 4 f
    ((3 :: Int) @=?) =<< Cache.requestOr ch 3 f
  it "drops caches" $ do
    ch <- new
      (Cache.Config $ Thyme.fromSeconds (0 :: Int))
    ir <- newIORef True
    let
      f _ = do
        firstTime <- readIORef ir
        if firstTime then writeIORef ir False >> pure 0 else pure 1
    ((0 :: Int) @=?)
      =<< Cache.requestOrInternal ch (Thyme.fromSeconds (0 :: Int)) () f
    ((1 :: Int) @=?)
      =<< Cache.requestOrInternal ch (Thyme.fromSeconds (2 :: Int)) () f
  it "handles exceptions" $ do
    ch <- new (Cache.Config d) :: IO (Cache.Handle Int Int)
    let f _ = error "foo"
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
