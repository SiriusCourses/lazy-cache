{-# LANGUAGE RecursiveDo #-}
-- | Caching based on the GHC heap object properties and  relies on lazyness implementation
-- in the GHC RTS. This approach does it's best effort for avoiding the duplicated work, 
-- however it the pathological cases it's possible that two theads will run the computation
-- on the same input concurrently. We have not observed this case in practice, but in the
-- case if you need strict guarantees on this property you should use "System.Cache.Impl.MVar"
-- instead.
--
-- This is battle-tested implementation. That fits all the properties that are required by the
-- interface.
--
module System.Cache.Impl.Ghc
  ( new
  ) where

import Control.Exception
import Data.Hashable
import Data.HashPSQ as PSQ
import Data.IORef
import Data.Tuple
import Control.Monad
import System.Clock.Seconds

import System.Cache.Internal.Interface

-- | We need our internal values to be even more lazy, in order
-- to prevent early evaluation and deadlock.
data Lazy a = Lazy { getLazy :: a}

-- | Creates new cache Handle. Keeps priotity queue from @psqueues@ package inside.
--
-- Properties:
--
--  * if multiple threads are running an IO action with the same input
--    concurrently then only one (with the best effort) will run, all
--    the rest will wait for that action and will either return a value,
--    or throw an exception depending on the the result of the first one
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
new Config {..} = do
  ref <- newIORef PSQ.empty
  pure $ Handle
    { requestOrInternal = \tm k f -> mdo
      mbox <-
        atomicModifyIORef ref
        $ swap
        . PSQ.alter
            (\case
              Just (p, ~v) | p >= diffTimeSpec tm configLongestAge ->
                (Just v, Just (p, v))
              _ -> (Nothing, Just (tm, Lazy eresult))
            )
            k
      eresult <- try $ maybe
        (f k)
        (\r -> do
          evaluate (getLazy r) >>= \case
            Left s -> throwIO s
            Right x -> pure x
        )
        mbox
      result <- case eresult of
        Left (s::SomeException) -> do
          atomicModifyIORef' ref $ \v -> (PSQ.delete k v,  ())
          throwIO s
        Right x -> pure x
      atomicModifyIORef' ref $ swap . PSQ.alterMin
        (\case
          Nothing -> ((), Nothing)
          Just (kk, p, v)
            | p < diffTimeSpec tm configLongestAge -> ((), Nothing)
            | otherwise -> ((), Just (kk, p, v))
        )
      pure result
    , remove = \k -> do
       void $ atomicModifyIORef ref $ swap . PSQ.alter (const ((), Nothing)) k
    , getClockTime = getTime configClock
    }
