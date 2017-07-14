{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Celtchar.Novel.Ogmarkup where

import Text.Ogmarkup
import Data.Text (Text, append)
import Data.String
import Text.Shakespeare.Text

import Celtchar.Novel.Structure (Language(..))

data NovConf = NovConf Language

el :: Text
el = "\n\n"

blk :: Text
    -> Text
blk = (`append` el)

instance GenConf NovConf Text where
    typography (NovConf French) = frenchTypo
    typography (NovConf English) = englishTypo

    printSpace _ None = ""
    printSpace _ Normal = " "
    printSpace _ Nbsp = "~"

    betweenDialogue _ = el

    storyTemplate _ sec = blk [st|\paragraph{} #{sec}|]

    paragraphTemplate _ = blk

    dialogueTemplate _ _ txt = [st|"\dialogue{}#{txt}|]
    thoughtTemplate _ _ txt = [st|\thought{}#{txt}|]
    replyTemplate _ txt = [st|\reply{#{txt}}|]

    strongEmphTemplate _ txt = [st|"\textbf{#{txt}}"|]
    emphTemplate _ txt = [st|"\textit{#{txt}}"|]

parseDoc :: Text -> Text
parseDoc doc = ogmarkup doc NovConf
