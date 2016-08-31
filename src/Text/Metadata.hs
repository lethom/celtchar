module Text.Metadata
  ( parseMetadata
  ) where

import Text.ParserCombinators.Parsec hiding (parse)

type MetadataParser = GenParser Char ()

parseMetadata :: String -> String -> Either ParseError (String, String)
parseMetadata = runParser (do
    spaces
    many1 $ char '-'
    spaces
    metadata <- manyTill anyToken
                         (do char '\n'
                             many1 $ char '-')
    spaces
    text <- manyTill anyToken eof
    return (metadata, text))
    ()
