{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Options.Generic
import System.IO (stderr, stdout, Handle, IOMode(..), openFile, hClose, hPutStrLn)
import System.FilePath (takeDirectory, (</>))
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified Data.Text.IO as T
import Data.FileEmbed
import System.Exit (exitFailure)

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
    f <- getNovelStructure $ conf

    let inDir = takeDirectory $ root cmd
        outDir = takeDirectory $ maybe "." id (output cmd)
    rootDir <- getCurrentDirectory

    -- write the final tex file
    setCurrentDirectory inDir

    case f of Right x  -> do
                res <- stringify (language x) (novelify x)

                setCurrentDirectory rootDir
                h <- getOutputHandle $ output cmd
                T.hPutStr h res
                hClose h

                T.writeFile (outDir </> "ogma.sty") $(embedStringFile "assets/ogma.sty")
              Left err -> do
                hPutStrLn stderr err
                exitFailure
