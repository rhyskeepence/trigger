{-# LANGUAGE RecordWildCards #-}

module Watcher
  ( watch
  ) where

import           Data.String          (fromString)
import           Parser
import           Protolude
import           System.Directory
import           System.FilePath
import qualified System.FilePath.Glob as G
import qualified System.FSNotify      as FS
import qualified Twitch               as T

type Handler = FilePath -> IO ()

watch :: Config -> Handler -> IO FS.WatchManager
watch config handler = do
  currentDir <- getCurrentDirectory
  baseDirectories <- mapM toAbsoluteDirectory (_dirs config)
  directoriesToWatch <- getDirectoriesToWatch baseDirectories
  let ignoringHandler = handlerWithIgnore (ignoredFilePatterns currentDir config) handler
  T.runWithConfig currentDir (twitchConfig directoriesToWatch) $ registerAllHandlers config ignoringHandler

registerAllHandlers :: Config -> Handler -> T.Dep
registerAllHandlers config handler = mapM_ (registerHandler handler) (allFilePaths config)

registerHandler :: Handler -> FilePath -> T.Dep
registerHandler handler fileGlob = T.addModify handler (fromString $ toS fileGlob)

handlerWithIgnore :: [G.Pattern] -> Handler -> Handler
handlerWithIgnore ignored handler filePath =
  if any (\pattern -> G.match pattern filePath) ignored
    then return ()
    else (handler filePath)

ignoredFilePatterns :: FilePath -> Config -> [G.Pattern]
ignoredFilePatterns currentDirectory Config {..} =
  let
    absoluteIgnoreDir :: Text -> [FilePath]
    absoluteIgnoreDir dir = createFilePaths (toS (currentDirectory </> toS dir)) (concat _ignore)
    allIgnored = foldl (\acc dir -> acc ++ (absoluteIgnoreDir dir)) [] _dirs
  in map (G.compile) allIgnored

allFilePaths :: Config -> [FilePath]
allFilePaths Config {..} = foldl (\acc dir -> acc ++ createFilePaths dir _files) [] _dirs

createFilePaths :: Text -> [Text] -> [FilePath]
createFilePaths dir = map (\file -> toS dir </> toS file)

watchConfig :: FS.WatchConfig
watchConfig = FS.WatchConfig {FS.confDebounce = FS.DebounceDefault, FS.confPollInterval = 0, FS.confUsePolling = False}

twitchConfig :: [FilePath] -> T.Config
twitchConfig dirsToWatch = T.Config {logger = const $ return (), dirs = dirsToWatch, watchConfig = watchConfig}

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
