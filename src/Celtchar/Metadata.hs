module Celtchar.Metadata
  ( parseMetadata
  ) where

import Text.ParserCombinators.Parsec hiding (parse)

type MetadataParser = GenParser Char ()

parseMetadata :: String -> String -> Either ParseError (Maybe String, String)
parseMetadata = runParser (do
    metadata <- (try parseMetadata) <|> (return Nothing)
    text <- manyTill anyToken eof
    return (metadata, text))
                          ()

  where
    parseMetadata = do
      spaces
      many1 $ char '-'
      char '\n'
      spaces
      metadata <- manyTill anyToken
                           (try $ do char '\n'
                                     many1 $ char '-'
                                     char '\n')
      spaces
      return $ Just metadata
