{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Parser
import Protolude
import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "can parse example.yaml" $ do
    config <- loadAndParse "example/example.yaml"
    config `shouldBe` 
      [ Config ["**/*.hs"] (Just ["stack build"]) (Just ["stack exec"])
      , Config ["**/*.elm"] (Just ["elm-make"]) Nothing]
