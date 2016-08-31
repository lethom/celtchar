{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Novel where

import           Text.Metadata
import           Control.Monad.State.Strict
import           Data.Monoid
import           Text.Novel.Structure
import           Text.Novel.Ogmarkup

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
        Right (metadata, txt) -> append $ parseDoc txt
        Left _                -> append $ "error while parsing " ++ path

instance Novelify Chapter where
    novelify c = do
      appendLn $ "\\chapter{" ++ chapterTitle c ++ "}"
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
      appendLn "\\documentclass{book}"
      appendLn "\\usepackage[french]{babel}"
      appendLn "\\usepackage[T1]{fontenc}"
      appendLn "\\usepackage[utf8]{inputenc}"
      appendLn "\\begin{document}"
      novelify $ manuscript n
      append "\\end{document}"
