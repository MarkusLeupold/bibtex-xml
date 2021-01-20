module BibTeX.Parser (parse) where

import BibTeX.Types
import Prelude
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Char as Char
import Data.String.Utils (lstrip, rstrip)


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
           "masterthesis"  -> (Left Masterthesis, r)
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


parseFieldName :: String -> (Maybe FieldName, String)
parseFieldName s = let (t, r) = span Char.isAlpha s
                       t_lc   = map Char.toLower t
                   in
                   case t_lc of
                   ""             -> (Nothing, r)
                   "address"      -> (Just Address, r)
                   "author"       -> (Just Author, r)
                   "booktitle"    -> (Just Booktitle, r)
                   "chaper"       -> (Just Chapter, r)
                   "edition"      -> (Just Edition, r)
                   "editor"       -> (Just Editor, r)
                   "howpublished" -> (Just Howpublished, r)
                   "institution"  -> (Just Institution, r)
                   "isbn"         -> (Just Isbn, r)
                   "journal"      -> (Just Journal, r)
                   "month"        -> (Just Month, r)
                   "note"         -> (Just Note, r)
                   "number"       -> (Just Number, r)
                   "organization" -> (Just Organization, r)
                   "pages"        -> (Just Pages, r)
                   "publisher"    -> (Just Publisher, r)
                   "school"       -> (Just School, r)
                   "series"       -> (Just Series, r)
                   "title"        -> (Just Title, r)
                   "type"         -> (Just Type, r)
                   "volume"       -> (Just Volume, r)
                   "year"         -> (Just Year, r)
                   _              -> (Just (UnknownField t_lc), r)

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


parseTag :: String -> (Maybe (FieldName, Value), String)
parseTag s = let (t, r) = parseFieldName (lstrip s) in
             case t of
             -- If t is Nothing, there wasn't found any tag type. We assume that
             -- the tag is empty and therefore evaluate to Nothing and the
             -- remaining input stream.
             Nothing -> (Nothing, r)
             -- If a tag type has been found, look for an equals character
             -- (i.e. '='), optionally preceded by some whitespace. If found,
             -- start parsing the tag value. Otherwise, raise an error.
             Just t' -> case lstrip r of
                        '=':r' -> let (v, r'') = parseValue r' in
                                  (Just (t', v), r'')
                        c:cs   -> error_midway
                                      (c:cs)
                                      $ "parseTag{BibToXml}: found '"
                                        ++ [c]
                                        ++ "' when expecting the following "
                                        ++ "character: '='"
                        ""     -> error $ "parseTag{BibToXml}: found " ++
                                          "end of string when expecting " ++
                                          "the following character: '='"

parseFields :: String -> (Fields, String)
parseFields s = let (mtv, r) = parseTag s
                    r'       = lstrip r
                in
                case r' of
                -- If we encounter a comma, then there could be another tag. Union
                -- the current tag (i.e. mtv) with all following Fields.
                ',':cs -> let (m, r'') = parseFields cs in
                          ( maybe m (\ (t, v) -> Map.insert t v m) mtv
                          , r''
                          )
                -- If the first character of r' is not a comma, the tag list must
                -- have ended. Evaluate to the empty Map if the current tag (i.e.
                -- mtv) is Nothing or, otherwise, to a Map containing only the
                -- current tag.
                _      -> ( maybe Map.empty
                                  (\ (t, v) -> Map.insert t v Map.empty)
                                  mtv
                          , r'
                          )


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


parseDataBlock :: String -> (Element, String)
parseDataBlock s =
    let (t, s')     = parseDataBlockType s
        afterBrace  = stripDelimiter '{' (True,False) s'
    in
    case t of
        Left t ->
            let (key, afterKey)   = parseEntryKey afterBrace
                afterComma        = stripDelimiter ',' (True,False) afterKey
                (fields, afterFields) = parseFields afterComma
                remaining         = stripDelimiter '}' (True,False) afterFields
            in (Entry t key fields, remaining)
        Right "string" ->
            let (m, afterDecl) = parseStringDecl afterBrace
                remaining      = stripDelimiter '}' (True,False) afterDecl
            in (StringDecl m, remaining)


parse :: String -> Database
parse s = case lstrip s of
          []     -> emptyDatabase
          '@':cs -> let (e, r) = parseDataBlock cs
                        es = parse r
                    in pushFront e es
          s      -> let (s', r) = span (/= '@') s
                    in pushFront (Comment (rstrip s')) (parse r)


error_midway remaining message = error $ message
                                         ++ "\nAt:\n"
                                         ++ case take 50 remaining of
                                                "" -> "\\0"
                                                s  -> s
