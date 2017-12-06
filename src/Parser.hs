{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Parser 
  ( Config (..)
  , loadAndParse ) where

import Protolude
import qualified Data.Yaml as Y
import Data.Aeson.Types

data Config = Config {
  _files :: [Text]
, _tasks :: Maybe [Text]
, _run :: Maybe [Text]  
} deriving (Eq, Show, Generic)

instance Y.FromJSON Config where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


loadAndParse :: FilePath -> IO [Config]
loadAndParse filePath = do
  config <- Y.decodeFileEither filePath 
  eitherToIo config
  where
    eitherToIo (Left configError) = throwIO configError
    eitherToIo (Right config)     = return config
