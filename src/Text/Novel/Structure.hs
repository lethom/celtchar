{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Novel.Structure where

import Data.Yaml
import GHC.Generics

data Document = Document FilePath
  deriving (Generic, Show)

instance FromJSON Document where
    parseJSON v = Document <$> parseJSON v

data Chapter = Chapter { chapterTitle :: String
                       , documents    :: [Document]
                       }
  deriving (Generic, Show)
instance FromJSON Chapter where
    parseJSON (Object v) = Chapter <$> v .: "title"
                                   <*> v .: "documents"

data Part = Part { partTitle :: String
                 , chapters  :: [Chapter]
                 }
  deriving (Generic, Show)
instance FromJSON Part where
    parseJSON (Object v) = Part <$> v .: "title"
                                <*> v .: "chapters"

data Manuscript = Manuscript [Part]
  deriving (Generic, Show)

instance FromJSON Manuscript where
    parseJSON v = Manuscript <$> parseJSON v

data Novel = Novel { author :: String
                   , novelTitle :: String
                   , manuscript :: Manuscript }
  deriving (Generic, Show)
instance FromJSON Novel where
    parseJSON (Object v) = Novel <$> v .: "author"
                                 <*> v .: "title"
                                 <*> v .: "manuscript"

getNovelStructure :: FilePath -> IO (Maybe Novel)
getNovelStructure = decodeFile
