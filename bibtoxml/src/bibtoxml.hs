module Main where

import Prelude
import BibTeX.Parser
import Data.String.Utils (strip)

main = do putStrLn "Please input the file path of the BibTeX file:"
          file   <- getLine >>= return . strip >>= readFile
          putStr "This is the parsed file:\n\n"
          print $ parse file
          putStr "\n"
