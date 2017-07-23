{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Celtchar.Novel.Structure where

import Data.Yaml
import GHC.Generics

data Language = French | English
  deriving (Generic)

data Document = Document FilePath
  deriving (Generic, Show)

instance FromJSON Document where
    parseJSON v = Document <$> parseJSON v

data Chapter = Chapter { chapterTitle :: Maybe String
                       , documents    :: [Document]
                       }
  deriving (Generic, Show)
instance FromJSON Chapter where
    parseJSON (Object v) = Chapter <$> v .:? "title"
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

instance FromJSON Language where
    parseJSON (String "english") = pure English
    parseJSON (String "french")  = pure French
    parseJSON _                  = fail "unknown language"

instance Show Language where
    show English = "english"
    show French  = "french"

data Novel = Novel { author      :: String
                   , language    :: Language
                   , novelTitle  :: String
                   , frontmatter :: Maybe [Chapter]
                   , manuscript  :: Manuscript
                   , appendix    :: Maybe [Chapter]
                   }
  deriving (Generic, Show)

instance FromJSON Novel where
    parseJSON (Object v) = Novel <$> v .: "author"
                                 <*> v .: "language"
                                 <*> v .: "title"
                                 <*> v .:? "frontmatter"
                                 <*> v .: "manuscript"
                                 <*> v .:? "appendix"

getNovelStructure :: FilePath -> IO (Either String Novel)
getNovelStructure conf = do
  ec <- decodeFileEither conf
  case ec of
    Right novel ->
      pure (Right novel)
    Left ex -> do
      pure (Left $ prettyPrintParseException ex)
