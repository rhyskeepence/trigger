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
  currentDir <- getCurrentDirectory
  baseDirectories <- mapM toAbsoluteDirectory (_dirs config)
  directoriesToWatch <- getDirectoriesToWatch baseDirectories
  T.runWithConfig currentDir (twitchConfig directoriesToWatch) $ registerAllHandlers config handler

registerAllHandlers :: Config -> Handler -> T.Dep
registerAllHandlers config handler = mapM_ (registerHandler handler) (allFilePaths config)

registerHandler :: Handler -> FilePath -> T.Dep
registerHandler handler fileGlob = T.addModify handler (fromString $ toS fileGlob)

allFilePaths :: Config -> [FilePath]
allFilePaths Config {..} =
  let createFilePaths :: Text -> [FilePath]
      createFilePaths dir = map (\file -> toS dir </> toS file) _files
  in foldl (\acc dir -> acc ++ createFilePaths dir) [] _dirs

watchConfig :: FS.WatchConfig
watchConfig =
  FS.WatchConfig
  {FS.confDebounce = FS.Debounce $ fromRational 200000, FS.confPollInterval = 0, FS.confUsePolling = False}

twitchConfig :: [FilePath] -> T.Config
twitchConfig dirsToWatch = T.Config {logger = print, dirs = dirsToWatch, watchConfig = watchConfig}

toAbsoluteDirectory :: Text -> IO FilePath
toAbsoluteDirectory filePath = do
  currentDir <- getCurrentDirectory
  makeAbsolute $ currentDir </> toS filePath

getDirectoriesToWatch :: [FilePath] -> IO [FilePath]
getDirectoriesToWatch = foldMap getDirectories
  where
    getDirectories baseDirectory = do
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
