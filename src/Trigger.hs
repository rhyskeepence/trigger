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
configToDep config =
  mapM_ (fileGlobToDep config) (_files config)

fileGlobToDep :: Config -> Text -> T.Dep
fileGlobToDep config fileGlob =
  T.addModify
    (handleChange config)
    (fromString $ toS fileGlob)

handleChange :: Config -> FilePath -> IO ()
handleChange config _ =
  runAll (_tasks config)

runAll :: Maybe [Text] -> IO ()
runAll tasks =
  mapM_ (P.system . toS) (concat tasks)
