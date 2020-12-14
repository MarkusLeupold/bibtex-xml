module Main where

import BibTeX.Parser as BT
import qualified BibTeX.Output.XML as BTXML (toElement)
import qualified Text.XML.Light as XML (ppElement)
import Data.String.Utils (strip)

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)


main = getArgs >>= run >> exitSuccess

xml_header :: String
xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"

convert :: String -> String
convert s = xml_header
            ++ "\n"
            ++ ( XML.ppElement $ BTXML.toElement $ BT.parse s )
            ++ "\n"

processInput :: [String] -> IO String
processInput [fpath] = do bibtex <- readFile fpath
                          return $ convert bibtex
processInput []      = do bibtex <- getContents
                          return $ convert bibtex
processInput _       = usage

run :: [String] -> IO ()
run ("-o":f:as) = do result <- processInput as
                     writeFile f result
run as          = do result <- processInput as
                     putStrLn result

usage = do progName <- getProgName
           putStrLn $ "Usage: " ++ progName ++
                      " [-o <output file>] [<input file>]"
           exitSuccess

