{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Parser 
  ( Config (..)
  , loadAndParse ) where

import Protolude
import qualified Data.Yaml as Y

data Config = Config {
  files :: [Text]
, tasks :: Maybe [Text]
, run :: Maybe [Text]  
} deriving (Eq, Show, Generic, Y.FromJSON)

loadAndParse :: FilePath -> IO [Config]
loadAndParse filePath = do
  config <- Y.decodeFileEither filePath 
  eitherToIo config
  where
    eitherToIo (Left configError) = throwIO configError
    eitherToIo (Right config)     = return config
