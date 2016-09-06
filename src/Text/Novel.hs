{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Novel where

import           Text.Metadata
import           Control.Monad.State.Strict
import           Data.Monoid
import           Data.Maybe
import           Text.Novel.Structure
import           Text.Novel.Ogmarkup
import           System.FilePath
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.LaTeX
import           Text.Pandoc.Options
import           Data.Default

type Builder = StateT String IO

append :: String -> Builder ()
append str = do st <- get
                put (st `mappend` str)

appendLn :: String -> Builder ()
appendLn str = do append str
                  append "\n"

stringify :: Builder () -> IO String
stringify builder = execStateT builder ""

class Novelify a where
    novelify :: a -> Builder ()

instance (Novelify a) => Novelify [a] where
    novelify (a:r) = do novelify a
                        novelify r
    novelify [] = return ()

instance Novelify Document where
    novelify (Document path) = do
      f <- liftIO $ readFile path
      case parseMetadata path f of
        Right (metadata, txt) -> appendLn $ case takeExtension path of
                                              ".up"  -> parseDoc txt
                                              ".tex" -> txt
                                              ".md"  -> parseMd f txt
                                              _      -> "\\begin{verbatim}\n" ++ txt ++ "\n\\end{verbatim}\n"
        Left _                -> appendLn $ "error while parsing " ++ path

instance Novelify Chapter where
    novelify c = do
      appendLn $ "\\chapter{" ++ (maybe "" id $ chapterTitle c) ++ "}"
      novelify $ documents c

instance Novelify Part where
    novelify p = do
      appendLn $ "\\part{" ++ partTitle p ++ "}"
      novelify $ chapters p

instance Novelify Manuscript where
    novelify (Manuscript m) = do
      append "\\mainmatter\n"
      novelify m

instance Novelify Novel where
    novelify n = do
      appendLn "\\documentclass[b5paper,12pt]{memoir}"
      appendLn "\\usepackage[french]{babel}"
      appendLn "\\usepackage[T1]{fontenc}"
      appendLn "\\usepackage[utf8]{inputenc}"
      appendLn "\\usepackage[urw-garamond]{mathdesign}"
      appendLn "\\usepackage{ogma}"
      appendLn "\\sloppy"
      appendLn $ "\\title{" ++ novelTitle n ++ "}"
      appendLn $ "\\author{" ++ author n ++ "}"
      appendLn "\\begin{document}"
      appendLn "\\maketitle"
      novelify $ manuscript n
      append "\\end{document}"

parseMd :: String -> String -> String
parseMd file txt = case readMarkdown ropts txt of Right pdc -> writeLaTeX wopts pdc
                                                  Left _    -> "error while compiling " ++ file
    where ropts :: ReaderOptions
          ropts = def

          wopts :: WriterOptions
          wopts = def
