{-# LANGUAGE QuasiQuotes #-}

module MetadataSpec where

import           Celtchar.Metadata
import           Test.Hspec
import           Text.RawString.QQ

spec :: Spec
spec = do describe "Metadata" $ do
            it "should deal with a file header" $
              parseMetadata "[spec]" fileWithMetadata
                `shouldBe` Right (Just "meta: data", "This is a test")

            it "should deal with a file without header" $
              parseMetadata "[spec]" fileWithoutMetadata
                `shouldBe` Right (Nothing, "This is a test")

fileWithMetadata :: String
fileWithMetadata = [r|-----
meta: data
------

This is a test|]

fileWithoutMetadata :: String
fileWithoutMetadata = "This is a test"
