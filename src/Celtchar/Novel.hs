{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Celtchar.Novel where

import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Default
import           Data.String
import           Data.Maybe
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import           Data.Monoid
import           System.FilePath
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.LaTeX
import           Text.Shakespeare.Text

import           Celtchar.Metadata
import           Celtchar.Novel.Ogmarkup
import           Celtchar.Novel.Structure

type Builder = StateT Text (ReaderT Language IO)

getLanguage :: Builder Language
getLanguage = lift $ ask

append :: Text -> Builder ()
append str = do st <- get
                put (st `mappend` str)

appendLn :: Text -> Builder ()
appendLn str = do append str
                  append "\n"

stringify :: Language -> Builder () -> IO Text
stringify lang builder = runReaderT (execStateT builder "") lang

class Novelify a where
    novelify :: a -> Builder ()

instance (Novelify a) => Novelify [a] where
    novelify (a:r) = do novelify a
                        novelify r
    novelify [] = return ()

instance Novelify Document where
    novelify (Document path) = do
      lang <- getLanguage
      f <- liftIO $ T.readFile path
      case parseMetadata path f of
        Right (metadata :: Maybe Text, txt) -> appendLn $ case takeExtension path of
                                                            ".up"  -> parseDoc lang txt
                                                            ".tex" -> txt
                                                            ".md"  -> parseMd f txt
                                                            _      -> verbatim txt
        Left _                -> appendLn $ "error while parsing " `mappend` (fromString path :: Text)
      where
        verbatim txt = [st|\begin{verbatim}
#{txt}
\end{verbatim}|]

instance Novelify Chapter where
    novelify c = do
      appendLn $ [st|\chapter{#{maybe "" id $ chapterTitle c}}|]
      novelify $ documents c

instance Novelify Part where
    novelify p = do
      appendLn $ [st|\part{#{partTitle p}}|]
      novelify $ chapters p

instance Novelify Manuscript where
    novelify (Manuscript m) = do
      append "\\mainmatter\n"
      novelify m

instance Novelify Novel where
    novelify n = do
      appendLn [st|\documentclass[b5paper,12pt]{memoir}
\usepackage[#{show $ language n}]{babel}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[urw-garamond]{mathdesign}
\usepackage{ogma}
\sloppy
\title{#{novelTitle n}}
\author{#{author n}}
\begin{document}
\maketitle|]
      novelify $ manuscript n
      append "\\end{document}"

parseMd :: Text
        -> Text
        -> Text
parseMd file txt = case readMarkdown ropts (unpack txt) of Right pdc -> pack $ writeLaTeX wopts pdc
                                                           Left _    -> [st|error while compiling #{file}|]
    where ropts :: ReaderOptions
          ropts = def

          wopts :: WriterOptions
          wopts = def
