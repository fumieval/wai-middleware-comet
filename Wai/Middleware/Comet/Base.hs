module Wai.Middleware.Comet.Base (Comet, new, wait, notify) where

import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

newtype Comet k = Comet (TVar (M.Map k Int))

new :: IO (Comet k)
new = Comet <$> newTVarIO M.empty

wait :: Ord k => Comet k -> k -> IO ()
wait (Comet v) k = do
    original <- readTVarIO v
    atomically $ do
      current <- readTVar v
      when (M.lookup k current == M.lookup k original) retry

notify :: Ord k => Comet k -> k -> IO ()
notify (Comet v) k = atomically $ modifyTVar' v $ M.insertWith (+) k 1
