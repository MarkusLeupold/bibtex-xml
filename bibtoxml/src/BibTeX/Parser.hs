module BibTeX.Parser ( parse
                     , parseUnique
                     , result
                     , remainder
                     , log
                     ) where

import BibTeX.Types
import Prelude hiding (log)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Char as Char
import Data.String.Utils (lstrip, rstrip, join)


type Log = [String]
empty_log = [] :: Log

data Result t = Success { result    :: t
                        , remainder :: String
                        , log       :: Log
                        }
              | Failure { log :: Log }
                deriving Show

stripDelimiter :: Char -> (Bool, Bool) -> String -> String
stripDelimiter d (l, r) s =
    let s' = if l then lstrip s else s
    in if null s' || (head s') /= d
           then error_midway s' $ "stripDelimiter{BibToXml}: "
                                  ++ "stripDelimiter _ " ++ show (l,r) ++ " _: "
                                  ++ "Can't find the delimiter '" ++ [d]
                                  ++ "' at the beginning of the input stream."
           else (if r then lstrip . tail else tail) s'


parseDataBlockType :: String -> (Either EntryType String, String)
parseDataBlockType s =
    let (t, r) = span Char.isAlpha s
        t_lc   = map Char.toLower t
    in case t_lc of
           "" -> error_midway s
                              $ "parseEntryType{BibToXml}: Missing data block "
                                ++ "type. Expecting at least one alphabetic "
                                ++ "character."
           "article"       -> (Left Article, r)
           "book"          -> (Left Book, r)
           "booklet"       -> (Left Booklet, r)
           "conference"    -> (Left Conference, r)
           "inbook"        -> (Left Inbook, r)
           "incollection"  -> (Left Incollection, r)
           "inproceedings" -> (Left Inproceedings, r)
           "manual"        -> (Left Manual, r)
           "mastersthesis" -> (Left Mastersthesis, r)
           "misc"          -> (Left Misc, r)
           "phdthesis"     -> (Left Phdthesis, r)
           "proceedings"   -> (Left Proceedings, r)
           "techreport"    -> (Left Techreport, r)
           "unpublished"   -> (Left Unpublished, r)
           "string"        -> (Right "string", r)
           _               -> (Left (UnknownEntry t_lc), r)

isEntryKeyFirstChar :: Char -> Bool
isEntryKeyFirstChar = Char.isAlpha
isEntryKeyLaterChar :: Char -> Bool
isEntryKeyLaterChar c = Char.isAlphaNum c || elem c "'-/:_"

parseEntryKey :: String -> (String, String)
parseEntryKey s = case lstrip s of
                  ""   -> error $ "parseEntryKey{BibToXml}: found end of " ++
                                  "string when expecting an alphabetic " ++
                                  "character"
                  c:cs -> if isEntryKeyFirstChar c
                          then let (p, r) = span isEntryKeyLaterChar cs
                               in (c:p, r)
                          else error_midway
                                   (c:cs)
                                   $ "parseEntryKey{BibToXml}: found '"
                                     ++ [c]
                                     ++ "' when expecting an alphabetic "
                                     ++ "character"

allFieldNames = [ "address", "author", "booktitle", "chaper", "edition"
                , "editor", "howpublished", "institution", "isbn", "journal"
                , "month", "note", "number", "organization", "pages"
                , "publisher", "school", "series", "title", "type", "volume"
                , "year"
                ]

stringToFieldName :: String -> Maybe FieldName
stringToFieldName s =
    let s_lc   = map Char.toLower s
    in  case s_lc of
        "address"      -> Just Address
        "author"       -> Just Author
        "booktitle"    -> Just Booktitle
        "chaper"       -> Just Chapter
        "edition"      -> Just Edition
        "editor"       -> Just Editor
        "howpublished" -> Just Howpublished
        "institution"  -> Just Institution
        "isbn"         -> Just Isbn
        "journal"      -> Just Journal
        "month"        -> Just Month
        "note"         -> Just Note
        "number"       -> Just Number
        "organization" -> Just Organization
        "pages"        -> Just Pages
        "publisher"    -> Just Publisher
        "school"       -> Just School
        "series"       -> Just Series
        "title"        -> Just Title
        "type"         -> Just Type
        "volume"       -> Just Volume
        "year"         -> Just Year
        _              -> Nothing

parseFieldName :: String -> Maybe (Result FieldName)
parseFieldName s =
    let (s', r) = span Char.isAlpha s
    in  if s' == ""
        then Nothing
        else Just $ maybe ( Failure [ "Error: Found no valid field name. "
                                      ++ "Expecting one of the following: "
                                      ++ join ", " allFieldNames ++ "\n"
                                      ++ "At the beginning of the stream \""
                                      ++ take 50 s ++ "...\""
                                    ]
                          )
                          ( \ n -> Success n r empty_log )
                          (stringToFieldName s')

parseValue :: String -> (Value, String)
parseValue = _parseValue [ numberParser
                         , referenceParser
                         , delimitedValueParser
                         ]

-- | a tuple describing a tag value parser
-- 1st element: predicate function to determine if the parser can be applied to
--              an input stream
-- 2nd element: the actual parser function which returns the raw parsed value
--              and the remaining input stream
-- 3rd element: constructor function to turn the raw value into a Value
type ValueParser = ( String -> Bool
                   , String -> (String, String)
                   , String -> Value
                   )

numberParser :: ValueParser
numberParser = ( \ s -> (not (null s)) && (Char.isDigit (head s))
               , span Char.isDigit
               , LiteralValue
               )

isStringNameChar :: Char -> Bool
isStringNameChar = Char.isAlpha

referenceParser :: ValueParser
referenceParser = ( \ s -> (not (null s)) && (isStringNameChar (head s))
                  , span isStringNameChar
                  , ReferencedValue
                  )

delimitedValueParser :: ValueParser
delimitedValueParser =
    ( \ s -> (not (null s)) && (elem (head s) "\"{")
    , \ (c:cs) -> let closingDelimiter = if c == '"' then '"' else '}'
                      (v, r) = _parseDelimitedValue closingDelimiter 0 cs
                  in if not (null r) && (head r) == closingDelimiter
                         then (v, tail r)
                         else error_midway
                                  r
                                  $ "delimitedValueParser{BibToXml}: could "
                                    ++ "not find expected closing delimiter '"
                                    ++ [closingDelimiter] ++ "'"
    , LiteralValue
    )

-- | parse a tag value using one of the given parsers
_parseValue :: [ ValueParser ]
            -> String
            -> (Value, String)
_parseValue ps cs =
    let cs' = lstrip cs
    in foldr ( \ (pr, f, cons) b ->
                   if pr cs'
                       then let (v_raw, r) = f cs'
                                v = cons v_raw :: Value
                            in case lstrip r of
                                   '#':rs -> let (v', r') = _parseValue ps rs
                                             in (ComposedValue v v', r')
                                   _      -> (v, r)
                            else b
             )
             ( error_midway cs $ "_parseValue{BibToXml}: Found no matching "
                                 ++ "parser function."
             )
             ps


-- | Value parser for literal values inside curly braces or quotes. The first
-- argument is the character which terminates the value, the second argument is
-- the current brace level inside the value. The surrounding delimiting braces
-- of BibTeX aren't counted by this argument, so the initial call should be
-- made with brace level zero.
-- This function expects the input stream to begin with the first character of
-- the value, i.e. the opening brace or quote must already have been removed.
_parseDelimitedValue :: Char -> Int -> String -> (String, String)
_parseDelimitedValue _ _ ""          = ("", "")

-- A backslash (i.e. the TeX escape character) introduces a TeX control
-- sequence. These are not touched by BibTeX, so we aren't allowed to interpret
-- the following character as a BibTeX control character. In the special case
-- that the following character is a curly brace, TeX won't interpret it as a
-- block beginning or end, because it is escaped.
-- Therefore, we can copy the character after the backslash to the result
-- without even looking at it and we are sure that we don't miss a brace level
-- change.
_parseDelimitedValue t b ('\\':c:cs) =
    let (v, r) = _parseDelimitedValue t b cs
    in ('\\':c:v, r)

-- Increment the brace level when encountering an opening curly brace (i.e. '{')
-- and continue parsing.
_parseDelimitedValue t b ('{':cs) =
    let (v, r) = _parseDelimitedValue t (b+1) cs
    in ('{':v, r)

-- While the brace level is zero, the terminating character means, that we have
-- reached the end of the value. If we encounter it, we put it back into the
-- input stream and return the empty string (i.e. the result value) together
-- with the remaining input stream.
-- Otherwise, put the character into the result and continue parsing. This will
-- also eat closing curly braces which have no matching opening brace.
_parseDelimitedValue t 0 (c:cs) =
    if t == c
        then ("", c:cs)
        else let (v, r) = _parseDelimitedValue t 0 cs
             in (c:v, r)

-- Decrement the brace level when encountering a closing curly brace (i.e. '}')
_parseDelimitedValue t b ('}':cs) =
    let (v, r) = _parseDelimitedValue t (b-1) cs
    in ('}':v, r)

-- When none of the special cases above match the input, simply continue
-- parsing.
_parseDelimitedValue t b (c:cs) =
    let (v, r) = _parseDelimitedValue t b cs
    in (c:v, r)


parseTag :: String -> Maybe (Result (FieldName, Value))
parseTag s =
    let logMessagePos = "While parsing the field at the beginning of "
                        ++ "the input stream \""
                        ++ take 50 s ++ "...\""
        maybeFnResult = parseFieldName (lstrip s)
    in  maybeFnResult >>=
        ( \ fnResult -> case fnResult of
            Failure l ->
                Just $ Failure { log = l ++ [logMessagePos] }
            -- If a field name has been found, look for an equals character
            -- (i.e. '='), optionally preceded by some whitespace. If found,
            -- start parsing the field value. Otherwise, raise an error.
            Success n r l ->
                case lstrip r of
                    '=':r' -> let (v, r'') = parseValue r' in
                                  Just $ Success (n, v) r'' l
                    c:cs ->
                        Just $ Failure $ l ++ [ "Error: found '"
                                                ++ [c]
                                                ++ "' when expecting the "
                                                ++ "following character: '='\n"
                                                ++ logMessagePos
                                              ]
                    "" ->
                        Just $ Failure $ l ++ [ "Error: Found end of string "
                                                ++ "when expecting the "
                                                ++ "following character: '='\n"
                                                ++ logMessagePos
                                              ]
        )

parseFields :: String -> Result Fields
parseFields s =
    let logMessagePos = "While parsing the fields of an entry at "
                        ++ "the beginning of the input stream \""
                        ++ take 50 s ++ "...\""
        maybeFResult = parseTag s
    in  case maybeFResult of
        Nothing                   -> parseFollowingFields s
        Just (Failure l)          -> Failure $ l ++ [logMessagePos]
        Just (Success (n, v) r l) ->
            let fsResult = parseFollowingFields r
            in  case fsResult of
                Failure _ -> fsResult
                Success fs r l_fs ->
                    fsResult { result = Map.insert n v fs }
    where
        parseFollowingFields s =
            case lstrip s of
                -- If we encounter a comma, then there could be another Field.
                -- Try to parse it.
                ',':cs ->  parseFields cs
                -- If the first character of (lstrip s) is not a comma, the
                -- field list must have ended. Result to the empty Map.
                _ -> Success Map.empty s empty_log


parseStringDecl :: String -> (StringMap, String)
parseStringDecl s =
    let (name, r) = span isStringNameChar (lstrip s)
    in if null name
           then (Map.empty, s)
           else let afterEquals       = stripDelimiter '=' (True,False) r
                    (val, afterValue) = parseValue afterEquals
                in case lstrip afterValue of
                       ',':r -> let (m, r') = parseStringDecl r
                                in (Map.insert name val m, r')
                       r     -> (Map.singleton name val, r)


parseDataBlock :: String -> Result Element
parseDataBlock s =
    let logMessagePos = "While parsing a data block at the "
                        ++ "beginning of the input stream "
                        ++ "\"" ++ take 50 s ++ "...\""
        (t, s')     = parseDataBlockType s
        afterBrace  = stripDelimiter '{' (True,False) s'
    in
    case t of
        Left (UnknownEntry t) ->
            Failure ["Unknown Entry type \"" ++ t ++ "\"", logMessagePos]
        Left t ->
            let (key, afterKey) = parseEntryKey afterBrace
                afterComma      = stripDelimiter ',' (True,False) afterKey
                fsResult        = parseFields afterComma
            in  case fsResult of
                Failure l ->
                    Failure $ l ++ [logMessagePos]
                Success fs r l ->
                    let remaining = stripDelimiter '}' (True,False) r
                    in  Success (Entry t key fs) remaining l
        Right "string" ->
            let (m, afterDecl) = parseStringDecl afterBrace
                remaining      = stripDelimiter '}' (True,False) afterDecl
            in Success (StringDecl m) remaining empty_log


parse :: String -> Result Database
parse s =
    case lstrip s of
        ""     -> Success { result    = emptyDatabase
                          , remainder = ""
                          , log       = empty_log
                          }
        '@':cs ->
            let elementResult = parseDataBlock cs in
                case elementResult of
                    Success e r l ->
                        let dbResult = parse r
                        in  dbResult { result = e : result dbResult
                                     , log    = l ++ log dbResult
                                     }
                    Failure l ->
                        let dbResult = parse (dropWhile (/='@') cs)
                        in  dbResult { log = l ++
                                             ( ( "Skipping to next '@' to try "
                                                 ++ "recovering from a "
                                                 ++ "previous error. The "
                                                 ++ "current entry will be "
                                                 ++ "dropped."
                                               ) : log dbResult
                                             )
                                     }
        s ->
            let (s', r)  = span (/= '@') s
                dbResult = parse r
            in  dbResult { result = Comment (rstrip s') : result dbResult }

parseUnique :: String -> Result Database
parseUnique s =
    let dbResult = parse s
        (db, l)  = eliminateDuplicates Set.empty $ result dbResult
    in  dbResult { result = db
                 , log = log dbResult ++ l
                 }
    where
        eliminateDuplicates :: (Set EntryKey) -> Database -> (Database, Log)
        eliminateDuplicates ids (e:es) =
            case e of
                Entry _ k _ ->
                    if Set.member k ids
                        then let (db, l) = eliminateDuplicates ids es
                             in  (db, (dup_mesg k) : l)
                        else let (db, l) = eliminateDuplicates
                                               (Set.insert k ids) es
                             in  (e:db, l)
                _ -> let (db, l) = eliminateDuplicates ids es
                     in  (e:db, l)
        eliminateDuplicates _ [] = (emptyDatabase, empty_log)
        dup_mesg k = "Error: Duplicate entry id: \"" ++ k
                     ++ "\". The entry will be dropped."


error_midway remaining message = error $ message
                                         ++ "\nAt:\n"
                                         ++ case take 50 remaining of
                                                "" -> "\\0"
                                                s  -> s
