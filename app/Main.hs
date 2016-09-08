{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Options.Generic
import Data.Text (unpack)
import qualified Data.Text.IO as T

import Celtchar.Novel.Structure
import Celtchar.Novel

data Command =
    Command { root :: FilePath }
  deriving (Generic, Show)

instance ParseRecord Command

main :: IO ()
main = do
    cmd <- getRecord "ogma-cli" :: IO Command

    f <- getNovelStructure $ root cmd
    case f of Just x  -> do res <- stringify (novelify x)
                            T.putStrLn $ res
              Nothing -> T.putStrLn "error while parsing"
