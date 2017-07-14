{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Options.Generic
import Data.Text (unpack)
import System.IO (stderr, stdout, Handle, IOMode(..), openFile, hClose)
import qualified Data.Text.IO as T

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

    case f of Just x  -> do res <- stringify (language x) (novelify x)
                            T.hPutStr h res
              Nothing -> T.hPutStrLn stderr "error while parsing"

    hClose h
