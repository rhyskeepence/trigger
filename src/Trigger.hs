{-# LANGUAGE RecordWildCards #-}

module Trigger
  ( run
  ) where

import           Console
import qualified Control.Monad.Catch as C
import           Parser
import           Protolude
import qualified System.Clock        as C
import qualified System.FSNotify     as FS
import qualified System.Process      as P
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
  putStrLn "Waiting..."
  _ <- getLine
  mapM_ FS.stopManager managers

runConfig :: MVar RunningProcesses -> Config -> IO FS.WatchManager
runConfig runningState config = do
  modifyMVar_ runningState (initialStartProcesses config)
  watch config (handleFileChange runningState config)

handleFileChange :: MVar RunningProcesses -> Config -> FilePath -> IO ()
handleFileChange runningState config file = do
  printFileChanged file
  modifyMVar_ runningState (restartProcesses config)

initialStartProcesses :: Config -> RunningProcesses -> IO RunningProcesses
initialStartProcesses Config {..} _ = mapM startProcess (concat _exec)

restartProcesses :: Config -> RunningProcesses -> IO RunningProcesses
restartProcesses config runningProcesses = do
  start <- C.getTime C.Monotonic
  mapM_ terminate runningProcesses
  processes <- attemptStart config
  end <- C.getTime C.Monotonic
  printCompleted start end
  threadDelay 200000
  return processes

attemptStart :: Config -> IO RunningProcesses
attemptStart Config {..} =
  swallowErrors $ do
    runTasks _tasks
    mapM startProcess (concat _exec)
  where
    swallowErrors :: IO RunningProcesses -> IO RunningProcesses
    swallowErrors = C.handleAll (\_ -> return [])

runTasks :: Maybe [Text] -> IO ()
runTasks tasks = mapM_ runTask (concat tasks)

runTask :: Text -> IO ()
runTask cmd = do
  printRunningTask cmd
  exitCode <- P.system $ toS cmd
  printTaskFinished exitCode
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure _ -> throwIO exitCode

startProcess :: Text -> IO RunningProcess
startProcess command = do
  (_, _, _, processHandle) <- P.createProcess_ (toS command) $ (P.shell (toS command)) { P.use_process_jobs = True
                                                                                       , P.create_group = True }
  printStartingRunTask command
  return $ RunningProcess command processHandle

terminate :: RunningProcess -> IO ()
terminate RunningProcess {..} = do
  printTerminatingRunTask cmd
  exit <- P.getProcessExitCode processHandle
  case exit of
    Nothing -> interrupt
    Just exitCode -> printAlreadyTerminated cmd exitCode
  where
    interrupt = do
      P.interruptProcessGroupOf processHandle
      threadDelay 50000
      exit <- P.getProcessExitCode processHandle
      case exit of
        Nothing -> interrupt
        Just _ -> printTerminated cmd
