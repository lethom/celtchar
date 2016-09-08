{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Options.Generic
import Data.Text (unpack)

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
                            putStrLn $ unpack res
              Nothing -> putStrLn "error while parsing"
