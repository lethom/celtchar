{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Novel.Ogmarkup where

import Text.Ogmarkup

data NovConf = NovConf

emptyLine :: String
emptyLine = "\n\n"

instance GenConf NovConf String where
    typography _ = frenchTypo

    printSpace _ None = ""
    printSpace _ Normal = " "
    printSpace _ Nbsp = "~"

    betweenDialogue _ = emptyLine

    storyTemplate _ sec = "\\paragraph{} " ++ sec ++ emptyLine

    paragraphTemplate _ = (++ emptyLine)

    dialogueTemplate _ _ txt = "\\dialogue{}" ++ txt
    thoughtTemplate _ _ txt = "\\thought{}" ++ txt
    replyTemplate _ txt = "\\reply{" ++ txt ++ "}"

    strongEmphTemplate _ txt = "\\textbf{" ++ txt ++ "}"
    emphTemplate _ txt = "\\textit{" ++ txt ++ "}"

parseDoc :: String -> String
parseDoc doc = ogmarkup ByLine doc NovConf
