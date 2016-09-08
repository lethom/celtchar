{-# LANGUAGE TypeFamilies #-}

module Celtchar.Metadata
  ( parseMetadata
  ) where

import Text.Megaparsec
import Data.String

parseMetadata :: (Stream a, Token a ~ Char, IsString b)
              => String
              -> a
              -> Either (ParseError Char Dec) (Maybe b, b)
parseMetadata = runParser (do
    metadata <- (try metadata) <|> (return Nothing)
    text <- manyTill anyChar eof
    return (metadata, fromString text))

  where
    metadata = do
      space
      some $ char '-'
      char '\n'
      space
      m <- manyTill anyChar (try $ do char '\n'
                                      some $ char '-'
                                      char '\n')
      space
      return $ Just $ fromString m
