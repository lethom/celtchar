{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Options.Generic
import Data.Text (unpack)
import System.IO (stderr, stdout, Handle, IOMode(..), openFile, hClose)
import System.FilePath (takeDirectory, (</>))
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified Data.Text.IO as T
import Data.FileEmbed

import Celtchar.Novel.Structure
import Celtchar.Novel

data Command =
    Command { root   :: FilePath
            , output :: Maybe FilePath
            }
  deriving (Generic, Show)

instance ParseRecord Command

getOutputHandle :: Maybe FilePath -> IO Handle
getOutputHandle Nothing = pure stdout
getOutputHandle (Just target) = openFile target WriteMode

main :: IO ()
main = do
    cmd <- getRecord "celtchar" :: IO Command

    let conf = root cmd
    h <- getOutputHandle $ output cmd
    f <- getNovelStructure $ conf

    let inDir = takeDirectory $ root cmd
        outDir = takeDirectory $ maybe "." id (output cmd)
    rootDir <- getCurrentDirectory

    -- write the final tex file
    setCurrentDirectory inDir

    case f of Just x  -> do res <- stringify (language x) (novelify x)
                            T.hPutStr h res
              Nothing -> T.hPutStrLn stderr "error while parsing"

    hClose h

    -- write the sty file
    setCurrentDirectory rootDir

    T.writeFile (outDir </> "ogma.sty") $(embedStringFile "ogma.sty")
