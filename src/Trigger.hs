{-# LANGUAGE RecordWildCards #-}

module Trigger
  ( run
  ) where

import           Console
import qualified Control.Arrow       as A
import qualified Control.Monad.Catch as C
import qualified Data.List           as L
import qualified Data.Text           as T
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
initialStartProcesses Config {..} _ = mapM startProcess (concat _run)

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
    mapM startProcess (concat _run)
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

startProcess :: RunConfig -> IO RunningProcess
startProcess RunConfig {..} = do
  (_, _, _, processHandle) <- P.createProcess_ (toS _command) $ process _workingDir _env _command
  printStartingRunTask _command
  return $ RunningProcess _command processHandle

terminate :: RunningProcess -> IO ()
terminate RunningProcess {..} = do
  printTerminatingRunTask cmd
  exit <- P.getProcessExitCode processHandle
  case exit of
    Nothing -> do
      P.terminateProcess processHandle
      exitCode <- P.waitForProcess processHandle
      printTerminated cmd exitCode
    Just exitCode -> printAlreadyTerminated cmd exitCode

process :: Maybe Text -> Maybe [(Text, Text)] -> Text -> P.CreateProcess
process workingDir env command =
  P.CreateProcess
  { cmdspec = splitCommand command
  , cwd = map toS workingDir
  , env = map (map (toS A.*** toS)) env
  , std_in = P.Inherit
  , std_out = P.Inherit
  , std_err = P.Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , detach_console = False
  , create_new_console = False
  , new_session = False
  , child_group = Nothing
  , child_user = Nothing
  }

splitCommand :: Text -> P.CmdSpec
splitCommand command =
  let cmdAndArgs = T.words command
      cmd = fromMaybe T.empty (head cmdAndArgs)
      args = L.tail cmdAndArgs
  in P.RawCommand (toS cmd) (map toS args)
