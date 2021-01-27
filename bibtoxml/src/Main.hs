module Main where

import BibTeX.Parser as BT
import BibTeX.Types as BT
import qualified BibTeX.Output.XML as BTXML ( toElement
                                            , toElementWithoutComments
                                            )
import qualified Text.XML.Light as XML (ppElement)
import Data.String.Utils (strip, join)

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)


main = getArgs >>= parseArgs >>= run >> exitSuccess

xml_header :: String
xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"

-- | convert raw BibTeX database content to a pretty-printed BibTeX-XML document
-- If the Bool is True, the result will contain the BibTeX comments as XML
-- elements. If it is False, comments will be dropped.
convert :: Bool -> BT.Database -> String
convert c db = xml_header
               ++ "\n"
               ++ ( XML.ppElement
                        $ ( if c
                                then BTXML.toElement
                                else BTXML.toElementWithoutComments
                          )
                              db
                  )
               ++ "\n"


data Args = Args { inputFile       :: Maybe String
                 , outputFile      :: Maybe String
                 , includeComments :: Bool
                 , logFile         :: Maybe String
                 } deriving Show

default_args = Args { inputFile       = Nothing
                    , outputFile      = Nothing
                    , includeComments = False
                    , logFile         = Nothing
                    }

parseArgs :: [String] -> IO Args
parseArgs = _parseArgs default_args

_parseArgs :: Args -> [String] -> IO Args
_parseArgs a ("-o":f:r)  = _parseArgs (a { outputFile      = Just f }) r
_parseArgs a ("-c":r)    = _parseArgs (a { includeComments = True   }) r
_parseArgs a ("-l":f:r)  = _parseArgs (a { logFile         = Just f }) r
_parseArgs _ ("-h":_)    = usage
-- If an argument does not match one of the flagged arg's patterns but begins
-- with a minus sign (i.e. '-') it can not have been used in the right way. So,
-- we execute usage.
_parseArgs _ (('-':_):_) = usage
_parseArgs a (f:r)       = maybe (_parseArgs (a { inputFile       = Just f }) r)
                                 (\ _ -> usage) -- two input file names
                                 (inputFile a)
_parseArgs a []          = return a


run :: Args -> IO ()
run args = do bibtex       <- maybe getContents readFile $ inputFile args
              parserResult <- return $ BT.parseUnique bibtex
              db           <- return $ BT.result parserResult
              result       <- return $ convert (includeComments args) db
              maybe (putStrLn  ::           String -> IO ())
                    (writeFile :: String -> String -> IO ())
                    (outputFile args)
                  $ result
              maybe (putStrLn  ::           String -> IO ())
                    (writeFile :: String -> String -> IO ())
                    (logFile args)
                  $ "== Parser log ================\n\n"
                    ++ (join "\n\n" $ BT.log parserResult)

usage :: IO a
usage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName
               ++ " [args] [<input file>] [args]\n"
               ++ "\n"
               ++ "Where args are zero or more of the following:\n"
               ++ " -c         Include BibTeX comments as XML elements\n"
               ++ " -h         Display this help message and exit\n"
               ++ " -l <file>  Output the log into <file>\n"
               ++ " -o <file>  Output the results into <file>\n"
               ++ "\n"
               ++ "If one of these args is specified more than once, only the\n"
               ++ "last one will be used. Also, currently only one input file\n"
               ++ "is supported.\n"
    exitSuccess
