{-# LANGUAGE RecordWildCards #-}

module Trigger
  ( run
  ) where

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
  putStrLn "Type anything to quit"
  _ <- getLine
  mapM_ FS.stopManager managers

runConfig :: MVar RunningProcesses -> Config -> IO FS.WatchManager
runConfig runningState config = watch config (handleFileChange runningState config)

handleFileChange :: MVar RunningProcesses -> Config -> FilePath -> IO ()
handleFileChange runningState config file = do
  putStrLn $ file ++ " changed"
  modifyMVar_ runningState (restartProcesses config)

restartProcesses :: Config -> RunningProcesses -> IO RunningProcesses
restartProcesses Config {..} runningProcesses = do
  mapM_ terminate runningProcesses
  runTasks _tasks
  mapM startProcess (concat _run)

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
