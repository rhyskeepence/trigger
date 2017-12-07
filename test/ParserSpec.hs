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
      [ Config "src" ["**/*.hs"] (Just ["stack build"]) (Just ["stack exec"])
      , Config "client" ["**/*.elm"] (Just ["elm-make"]) Nothing]
