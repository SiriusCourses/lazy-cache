-- | Safer cache version that is implemented totally
-- on the user level. Using 'MVar' as a synchronization
-- primitive.
--
-- While this implementation is slower in general case it may
-- be more resilient to changes in GHC and RTS. This implementation
-- is mostly used in order to sanity check the GHC one, and in order
-- to be able to switch to this one without program recompilation.
--
-- However if you need strict guarantees that the only one action with
-- the same input is run concurrently you should prefer this implementation.
module System.Cache.Impl.MVar
  ( new
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.HashPSQ as PSQ
import Data.Hashable
import Data.IORef
import Data.Tuple
import System.Clock.Seconds

import System.Cache.Internal.Interface

-- | Create new cache handle. Keeps priority queue from psqueues package inside.
--
-- Properties:
--
--  * if multiple threads are running an IO action with the same input
--    concurrently then only one will run, all the rest will wait for that
--    action and will either return a value, or throw an exception depending
--    on the the result of the first one
--  * storage is cleared only during access, it will remove redundant work
--    but may lead to the situation when cached values are stored in memory
--    for a longer period that it was expected
--  * psqueue structure uses both 'Hashable' and 'Ord' constraints and is
--    not vulnerable to the hash collision attacks.
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
                     | p >= tm - configLongestAge -> pure v
                     | otherwise -> updateLock cfg ref lock r tm k f
        atomicModifyIORef' ref $ swap . PSQ.alterMin
                (\case
                  Nothing -> ((), Nothing)
                  Just (kk, p, v)
                    | p < tm - configLongestAge -> ((), Nothing)
                    | otherwise -> ((), Just (kk, p, v))
                )
        pure result
    , remove = \k -> do
       void $ atomicModifyIORef ref $ swap . PSQ.alter (const ((), Nothing)) k
    , getClockTime = getTime configClock
    }

-- | There is no value in the queue, but someone may already trying to create a lock,
-- so we need to register a lock, verifying that it was registered concurrently.
insertElement :: (Hashable k, Ord k)
  => Config
  -> IORef (HashPSQ k Seconds (IORef (Maybe z), MVar (Maybe (Seconds, z))))
  -> Seconds
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
  -> IORef (HashPSQ k Seconds (IORef (Maybe z), MVar (Maybe (Seconds, z))))
  -> MVar (Maybe (Seconds, z))
  -> IORef (Maybe z)
  -> Seconds 
  -> k
  -> (k -> IO z)
  -> IO z
updateLock Config{..} ref lock inner tm k f  = modifyMVar lock $ \case
  Just x@(p, v) -- Result exists and is valid, we can just return it
    | p >= tm - configLongestAge -> pure (Just x, v)
  _ -> do -- There is no result or it's outdated, we can update it.
    value <- f k
    writeIORef inner (Just value)
    atomicModifyIORef ref (\queue -> (PSQ.insert k tm (inner,lock) queue, ()))
    pure (Just (tm, value), value)
