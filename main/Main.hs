module Main where

import Protolude

import Parser
import Trigger


main :: IO ()
main = do
  config <- loadAndParse "trigger.yaml"
  run config