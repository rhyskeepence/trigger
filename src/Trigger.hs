module Trigger 
  ( run 
  ) where


import Parser
import Protolude
import Data.String (fromString)
import qualified System.Process as P
import qualified Twitch as T    

twitchOpts :: T.Options
twitchOpts = T.Options T.NoLogger Nothing Nothing True T.DebounceDefault 0 0 False

run :: [Config] -> IO ()
run configs =
  T.defaultMainWithOptions twitchOpts $ sequenceA_ (configToDep <$> configs)


configToDep :: Config -> T.DepM ()  
configToDep (Config filesToWatch (Just tasksToRun) _) = 
  mapM_ (fileGlobToDep tasksToRun) filesToWatch

configToDep (Config _ Nothing _) = 
  return ()


fileGlobToDep :: [Text] -> Text -> T.Dep
fileGlobToDep tasksToRun filesToWatch = 
      T.addModify 
        (\_ -> mapM_ (P.system . toS) tasksToRun) 
        (fromString $ toS filesToWatch)
