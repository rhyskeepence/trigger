module Trigger 
  ( main 
  ) where


import Parser
import Protolude
import Data.String (fromString)
import qualified System.Process as P
import qualified Twitch as T    

twitchOpts :: T.Options
twitchOpts = T.Options T.NoLogger Nothing Nothing True T.DebounceDefault 0 0 False

main :: [Config] -> IO ()
main configs =
  T.defaultMainWithOptions twitchOpts $ sequenceA_ (configToDep <$> configs)


configToDep :: Config -> T.DepM ()  
configToDep (Config filesToWatch (Just tasksToRun) _) = 
  mapM_ (addDepForFileGlob . toS) filesToWatch
  where 
    addDepForFileGlob :: FilePath -> T.Dep 
    addDepForFileGlob fileGlob = 
      T.addModify 
        (\_ -> mapM_ (P.system . toS) tasksToRun) 
        (fromString fileGlob)

configToDep (Config _ Nothing _) = 
  return ()