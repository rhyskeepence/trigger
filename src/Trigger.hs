module Trigger
  ( run
  ) where

import           Data.String    (fromString)
import           Parser
import           Protolude
import qualified System.Process as P
import qualified Twitch         as T

twitchOpts :: T.Options
twitchOpts = T.Options T.NoLogger Nothing Nothing True T.DebounceDefault 0 0 False

run :: [Config] -> IO ()
run configs = T.defaultMainWithOptions twitchOpts $ mapM_ configToDep configs

configToDep :: Config -> T.DepM ()
configToDep (Config filesToWatch (Just tasksToRun) _) =
  mapM_ (fileGlobToDep tasksToRun) filesToWatch

configToDep (Config _ Nothing _) =
  return ()

fileGlobToDep :: [Text] -> Text -> T.Dep
fileGlobToDep tasksToRun filesToWatch =
  T.addModify
    (handleChange tasksToRun)
    (fromString $ toS filesToWatch)

handleChange :: [Text] -> FilePath -> IO ()
handleChange tasksToRun _ =
  mapM_ (P.system . toS) tasksToRun

