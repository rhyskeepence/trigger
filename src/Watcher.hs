{-# LANGUAGE RecordWildCards #-}

module Watcher
  ( watch
  ) where

import           Data.String      (fromString)
import           Parser
import           Protolude
import           System.Directory
import           System.FilePath
import qualified System.FSNotify  as FS
import qualified Twitch           as T

type Handler = FilePath -> IO ()

watch :: Config -> Handler -> IO FS.WatchManager
watch config handler = do
  baseDirectory <- getBaseDirectory config
  directoriesToWatch <- getDirectoriesToWatch baseDirectory
  T.runWithConfig baseDirectory (twitchConfig directoriesToWatch) $ registerAllHandlers config handler

registerAllHandlers :: Config -> Handler -> T.Dep
registerAllHandlers Config {..} handler = mapM_ (registerHandler handler) _files

registerHandler :: Handler -> Text -> T.Dep
registerHandler handler fileGlob = T.addModify handler (fromString $ toS fileGlob)

watchConfig :: FS.WatchConfig
watchConfig =
  FS.WatchConfig
  {FS.confDebounce = FS.Debounce $ fromRational 200000, FS.confPollInterval = 0, FS.confUsePolling = False}

twitchConfig :: [FilePath] -> T.Config
twitchConfig dirsToWatch = T.Config {logger = const $ return (), dirs = dirsToWatch, watchConfig = watchConfig}

getBaseDirectory :: Config -> IO FilePath
getBaseDirectory Config {..} = do
  currentDir <- getCurrentDirectory
  makeAbsolute $ currentDir </> toS _dir

getDirectoriesToWatch :: FilePath -> IO [FilePath]
getDirectoriesToWatch baseDirectory = do
  subDirs <- findAllDirs baseDirectory
  return $ baseDirectory : subDirs

findAllDirs :: FilePath -> IO [FilePath]
findAllDirs path = do
  dirs <- findImmediateDirs path
  nestedDirs <- mapM findAllDirs dirs
  return (dirs ++ concat nestedDirs)

findImmediateDirs :: FilePath -> IO [FilePath]
findImmediateDirs = getDirectoryContentsPath >=> filterM doesDirectoryExist >=> canonicalize
  where
    canonicalize :: [FilePath] -> IO [FilePath]
    canonicalize = mapM canonicalizeDirPath

getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath path = map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path

canonicalizeDirPath :: FilePath -> IO FilePath
canonicalizeDirPath path = addTrailingPathSeparator <$> canonicalizePath path
