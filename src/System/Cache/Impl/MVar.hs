module System.Cache.Impl.MVar
  ( new
  ) where

import Control.Exception
import Control.Concurrent
import Data.AdditiveGroup
import Data.Hashable
import Data.HashPSQ as PSQ
import Data.IORef
import Data.Thyme.Clock.POSIX
import Data.Tuple
import Control.Monad

import System.Cache.Internal.Interface

-- | Create new cache service.
--
new
  :: (Show a, Hashable a, Ord a)
  => Config
  -> IO (Handle a b)
new cfg@Config {..} = do
  ref <- newIORef PSQ.empty
  pure $ Handle
    { requestOrInternal = \tm k f -> do
        queue <- readIORef ref
        result
          <- case PSQ.lookup k queue of
               Nothing -> insertElement cfg ref tm k f
               Just (p, (r, lock)) -> do
                 readIORef r >>= \case
                   Nothing
                     -> updateLock cfg ref lock r tm k f
                   Just v
                     | p >= tm ^-^ configLongestAge -> pure v
                     | otherwise -> updateLock cfg ref lock r tm k f
        atomicModifyIORef' ref $ swap . PSQ.alterMin
                (\case
                  Nothing -> ((), Nothing)
                  Just (kk, p, v)
                    | p < tm ^-^ configLongestAge -> ((), Nothing)
                    | otherwise -> ((), Just (kk, p, v))
                )
        pure result
    , remove = \k -> do
       void $ atomicModifyIORef ref $ swap . PSQ.alter (const ((), Nothing)) k
    }

-- | There is no value in the queue, but someone may already trying to create a lock,
-- so we need to register a lock, verifying that it was registered concurrently.
insertElement :: (Hashable k, Ord k)
  => Config
  -> IORef (HashPSQ k POSIXTime (IORef (Maybe z), MVar (Maybe (POSIXTime, z))))
  -> POSIXTime
  -> k
  -> (k -> IO z)
  -> IO z
insertElement cfg ref tm k f = newEmptyMVar >>= \x -> go x `onException` (tryPutMVar x Nothing) where
  go our_lock = do
    result_box <- newIORef Nothing
    update_result
      <- atomicModifyIORef ref $ swap .
        PSQ.alter
          (\case
             Just x@(_, their_lock) -> (Just their_lock, Just x)
             Nothing -> (Nothing, Just (tm, (result_box, our_lock)))
          ) k
    case update_result of
      Just (r, their_lock)
        -> updateLock cfg ref their_lock r tm k f -- Someone else managed to insert the lock, first.
      Nothing -> do
        -- We are holding a lock, so we need to evaluate value and register it
        value <- f k
        putMVar our_lock (Just (tm, value))
        writeIORef result_box (Just value)
        pure value

updateLock :: (Hashable k, Ord k)
  => Config
  -> IORef (HashPSQ k POSIXTime (IORef (Maybe z), MVar (Maybe (POSIXTime, z))))
  -> MVar (Maybe (POSIXTime, z))
  -> IORef (Maybe z)
  -> POSIXTime
  -> k
  -> (k -> IO z)
  -> IO z
updateLock Config{..} ref lock inner tm k f  = modifyMVar lock $ \case
  Just x@(p, v) -- Result exists and is valid, we can just return it
    | p >= tm ^-^ configLongestAge -> pure (Just x, v)
  _ -> do -- There is no result or it's outdated, we can update it.
    value <- f k
    writeIORef inner (Just value)
    atomicModifyIORef ref (\queue -> (PSQ.insert k tm (inner,lock) queue, ()))
    pure (Just (tm, value), value)
