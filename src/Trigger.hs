{-# LANGUAGE RecordWildCards #-}

module Trigger
  ( run
  ) where

import           Console
import           Parser
import           Protolude
import qualified System.FSNotify as FS
import qualified System.Process  as P
import           Watcher

data RunningProcess = RunningProcess
  { cmd           :: Text
  , processHandle :: P.ProcessHandle
  }

type RunningProcesses = [RunningProcess]

run :: [Config] -> IO ()
run configs = do
  runningState <- newMVar []
  managers <- mapM (runConfig runningState) configs
  _ <- getLine
  mapM_ FS.stopManager managers

runConfig :: MVar RunningProcesses -> Config -> IO FS.WatchManager
runConfig runningState config = watch config (handleFileChange runningState config)

handleFileChange :: MVar RunningProcesses -> Config -> FilePath -> IO ()
handleFileChange runningState config file = do
  printFileChanged file
  modifyMVar_ runningState (restartProcesses config)


restartProcesses :: Config -> RunningProcesses -> IO RunningProcesses
restartProcesses Config {..} runningProcesses = do
  mapM_ terminate runningProcesses
  runTasks _tasks
  mapM startProcess (concat _run)


runTasks :: Maybe [Text] -> IO ()
runTasks tasks = mapM_ runProcess (concat tasks)

runProcess :: Text -> IO ()
runProcess cmd = do
  printRunningTask cmd
  exitCode <- P.system $ toS cmd
  printTaskFinished exitCode

startProcess :: Text -> IO RunningProcess
startProcess cmd = do
  processHandle <- P.spawnCommand $ toS cmd
  printStartingRunTask cmd
  return $ RunningProcess cmd processHandle

terminate :: RunningProcess -> IO ()
terminate RunningProcess {..} = do
  printTerminatingRunTask cmd
  exit <- P.getProcessExitCode processHandle
  case exit of
    Nothing -> do
      P.terminateProcess processHandle
      exitCode <- P.waitForProcess processHandle
      printTerminated cmd exitCode
    Just exitCode ->
      printAlreadyTerminated cmd exitCode
