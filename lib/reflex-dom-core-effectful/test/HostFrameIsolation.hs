module Main (main) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import Reflex.Effectful.DomHost (FireCommand (..), attachImmediateWidget)
import System.Timeout (timeout)

-- No DOM or postBuild action is needed: this checks the host frame boundary.
makeHost :: IO () -> IO FireCommand
makeHost before = snd <$> attachImmediateWidget (\_ _ -> do
  liftIO before
  postBuild <- liftIO $ newIORef Nothing
  pure ((), postBuild))

checkExclusion :: String -> IO () -> IO ()
checkExclusion label competing = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  withAsync (makeHost (putMVar entered () >> takeMVar release)) $ \first -> do
    takeMVar entered
    withAsync competing $ \second -> do
      premature <- timeout 100000 (wait second)
      putMVar release ()
      void (wait first >> wait second)
      unless (premature == Nothing) $ fail (label <> " overlapped construction")

main :: IO ()
main = do
  checkExclusion "another browser" $ void $ makeHost (pure ())
  command <- makeHost (pure ())
  checkExclusion "event draining" $ runFireCommand command []
