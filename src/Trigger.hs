{-# LANGUAGE RecordWildCards #-}

module Trigger
  ( run
  ) where

import           Data.String    (fromString)
import           Parser
import           Protolude
import           System.Directory
import           System.FilePath
import qualified System.Process as P
import qualified Twitch         as T
import qualified System.FSNotify as FS

run :: [Config] -> IO ()
run configs = do
  managers <- mapM runConfig configs
  putStrLn "Type anything to quit"
  _ <- getLine
  mapM_ FS.stopManager managers

runConfig :: Config -> IO FS.WatchManager
runConfig config = do
  root <- getBaseDirectory config
  putStrLn root
  c <- optionsToConfig root config
  T.runWithConfig root c $ configToDep config

configToDep :: Config -> T.Dep
configToDep config =
  mapM_ (fileGlobToDep config) (_files config)

fileGlobToDep :: Config -> Text -> T.Dep
fileGlobToDep config fileGlob =
  T.addModify
    (handleFileChange config)
    (fromString $ toS fileGlob)

handleFileChange :: Config -> FilePath -> IO ()
handleFileChange config files = do
  putStrLn $ files ++ " changed"
  runTasks (_tasks config)
  restartProcesses (_run config)

runTasks :: Maybe [Text] -> IO ()
runTasks tasks =
  mapM_ (P.system . toS) (concat tasks)

restartProcesses :: Maybe [Text] -> IO ()
restartProcesses processes =
  mapM_ (P.system . toS) (concat processes)


getBaseDirectory :: Config -> IO FilePath
getBaseDirectory Config {..} = do
  currentDir <- getCurrentDirectory
  makeAbsolute $ currentDir </> toS _dir

optionsToConfig :: FilePath -> Config -> IO T.Config
optionsToConfig baseDirectory Config {..} = do
  subDirs <- findAllDirs baseDirectory
  let dirsToWatch = baseDirectory : subDirs

  let watchConfig = FS.WatchConfig
        { FS.confDebounce     = FS.Debounce $ fromRational 200000
        , FS.confPollInterval = 10^(6 :: Int) -- 1 second
        , FS.confUsePolling   = False
        }

  let config = T.Config
        { logger      = print
        , dirs        = dirsToWatch
        , watchConfig = watchConfig
        }
  return config

findAllDirs :: FilePath -> IO [FilePath]
findAllDirs path = do
  dirs <- findImmediateDirs path
  print dirs
  nestedDirs <- mapM findAllDirs dirs
  print nestedDirs
  return (dirs ++ concat nestedDirs)

findImmediateDirs :: FilePath -> IO [FilePath]
findImmediateDirs = getDirectoryContentsPath >=> filterM doesDirectoryExist >=> canonicalize
  where
    canonicalize :: [FilePath] -> IO [FilePath]
    canonicalize = mapM canonicalizeDirPath


getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath path =
  map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path

canonicalizeDirPath :: FilePath -> IO FilePath
canonicalizeDirPath path = addTrailingPathSeparator <$> canonicalizePath path
