module Text.Metadata
  ( parseMetadata
  ) where

import Text.ParserCombinators.Parsec hiding (parse)

type MetadataParser = GenParser Char ()

parseMetadata :: String -> String -> Either ParseError (String, String)
parseMetadata = runParser (do
    spaces
    many1 $ char '-'
    char '\n'
    spaces
    metadata <- manyTill anyToken
                         (try $ do char '\n'
                                   many1 $ char '-'
                                   char '\n')
    spaces
    text <- manyTill anyToken eof
    return (metadata, text))
    ()
