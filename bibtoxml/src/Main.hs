module Main where

import Prelude
import BibTeX.Parser
import qualified BibTeX.Output.XML as BTXML (toElement)
import qualified Text.XML.Light as XML (ppElement)
import Data.String.Utils (strip)
import Data.List (intercalate)

main = do putStrLn "Please input the file path of the BibTeX file:"
          file   <- getLine >>= return . strip >>= readFile
          putStr "This is the parsed file:\n\n"
          es <- return $ parse file
          putStr $ XML.ppElement $ BTXML.toElement es
          putStr "\n"
