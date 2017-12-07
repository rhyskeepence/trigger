{-# LANGUAGE RecordWildCards #-}

module Trigger
  ( run
  ) where

import           Parser
import           Protolude
import qualified System.FSNotify  as FS
import qualified System.Process   as P
import           Watcher

data RunningProcess = RunningProcess
  { cmd :: Text
  , processHandle :: P.ProcessHandle
  }

data RunState = RunState
  { runningProcesses :: [RunningProcess]
  }

run :: [Config] -> IO ()
run configs = do
  runningState <- newMVar $ RunState []
  managers <- mapM (runConfig runningState) configs
  putStrLn "Type anything to quit"
  _ <- getLine
  mapM_ FS.stopManager managers

runConfig :: MVar RunState -> Config -> IO FS.WatchManager
runConfig runningState config = watch config (handleFileChange runningState config)

handleFileChange :: MVar RunState -> Config -> FilePath -> IO ()
handleFileChange runningState config file = do
  putStrLn $ file ++ " changed"
  modifyMVar_ runningState (restartProcesses config)

restartProcesses :: Config -> RunState -> IO RunState
restartProcesses Config {..} RunState {..} = do
  mapM_ terminate runningProcesses
  runTasks _tasks
  handles <- mapM startProcess (concat _run)
  return $ RunState handles

runTasks :: Maybe [Text] -> IO ()
runTasks tasks = mapM_ (P.system . toS) (concat tasks)

startProcess :: Text -> IO RunningProcess
startProcess cmd = do
  processHandle <- P.spawnCommand $ toS cmd
  putStrLn $ "Started: " <> toS cmd
  return $ RunningProcess cmd processHandle

terminate :: RunningProcess -> IO ()
terminate RunningProcess {..} = do
  putStrLn "Terminating"
  exit <- P.getProcessExitCode processHandle
  case exit of
    Nothing -> do
      P.terminateProcess processHandle
      exitCode <- P.waitForProcess processHandle
      putStrLn $ toS cmd <> " exited (" <> show exitCode <> ")"
    Just exitCode ->
      putStrLn $ toS cmd <> " had already terminated (" <> show exitCode <> toS ")"
