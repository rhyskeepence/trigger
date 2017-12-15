{-# LANGUAGE DeriveGeneric  #-}

module Parser
  ( Config(..)
  , RunConfig(..)
  , loadAndParse
  ) where

import           Data.Aeson.Types
import qualified Data.Yaml        as Y
import           Protolude

data Config = Config
  { _dirs :: [Text]
  , _files :: [Text]
  , _ignore :: Maybe [Text]
  , _tasks :: Maybe [Text]
  , _run   :: Maybe [RunConfig]
  } deriving (Eq, Show, Generic)

instance Y.FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data RunConfig = RunConfig
  { _workingDir :: Maybe Text
  , _command :: Text
  , _env :: Maybe [(Text,Text)]
  } deriving (Eq, Show, Generic)

instance Y.FromJSON RunConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

loadAndParse :: FilePath -> IO [Config]
loadAndParse filePath = do
  config <- Y.decodeFileEither filePath
  eitherToIo config
  where
    eitherToIo (Left configError) = throwIO configError
    eitherToIo (Right config)     = return config
