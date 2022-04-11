module Wai.Middleware.Comet.Base (Comet, new, wait, notify) where

import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

newtype Comet k = Comet (TVar (M.Map k State))

new :: IO (Comet k)
new = Comet <$> newTVarIO M.empty

data State = State
  { users :: !Int
  , counter :: !Int
  }

addUser :: State -> State
addUser s = s { users = users s + 1 }

wait :: Ord k => Comet k -> k -> IO ()
wait (Comet v) k = bracket init release $ \c0 -> atomically $ do
  c <- maybe 0 counter . M.lookup k <$> readTVar v
  when (c0 == c) retry
  where
    init = atomically $ do
      m <- readTVar v
      let s = maybe (State 1 0) (\s -> s { users = users s + 1 })
            $ M.lookup k m
      writeTVar v $ M.insert k s m
      pure $ counter s
    release _ = atomically $ do
      m <- readTVar v
      case M.lookup k m of
        Nothing -> pure () -- bug?
        Just (State n c)
          | n <= 1 -> writeTVar v $ M.delete k m
          | otherwise -> writeTVar v $ M.insert k (State (n - 1) c) m

notify :: Ord k => Comet k -> k -> IO ()
notify (Comet v) k = atomically $ modifyTVar' v $ M.adjust (\s -> s { counter = counter s + 1 }) k
