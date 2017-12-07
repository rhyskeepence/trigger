module Trigger
  ( run
  ) where

import           Parser
import           Protolude
import qualified System.FSNotify  as FS
import qualified System.Process   as P
import           Watcher

run :: [Config] -> IO ()
run configs = do
  managers <- mapM runConfig configs
  putStrLn "Type anything to quit"
  _ <- getLine
  mapM_ FS.stopManager managers

runConfig :: Config -> IO FS.WatchManager
runConfig config = watch config (handleFileChange config)

handleFileChange :: Config -> FilePath -> IO ()
handleFileChange config files = do
  putStrLn $ files ++ " changed"
  runTasks (_tasks config)
  restartProcesses (_run config)

runTasks :: Maybe [Text] -> IO ()
runTasks tasks = mapM_ (P.system . toS) (concat tasks)

restartProcesses :: Maybe [Text] -> IO ()
restartProcesses processes = mapM_ (P.system . toS) (concat processes)
