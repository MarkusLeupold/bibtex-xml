module BibTeX.Parser (parse) where

import BibTeX.Types
import Prelude
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Char as Char
import Data.String.Utils (lstrip)


parseEntryType :: String -> (EntryType, String)
parseEntryType s = let (t, r) = span Char.isAlpha s
                       t_lc   = map Char.toLower t
                   in
                   case t_lc of
                   "" -> error_midway
                             s
                             $ "parseEntryType{BibToXml}: Missing entry type. "
                               ++ "Expecting at least one alphabetic character."
                   "article"       -> (Article, r)
                   "book"          -> (Book, r)
                   "booklet"       -> (Booklet, r)
                   "conference"    -> (Conference, r)
                   "inbook"        -> (Inbook, r)
                   "incollection"  -> (Incollection, r)
                   "inproceedings" -> (Inproceedings, r)
                   "manual"        -> (Manual, r)
                   "masterthesis"  -> (Masterthesis, r)
                   "misc"          -> (Misc, r)
                   "phdthesis"     -> (Phdthesis, r)
                   "proceedings"   -> (Proceedings, r)
                   "techreport"    -> (Techreport, r)
                   "unpublished"   -> (Unpublished, r)
                   _               -> (UnknownEntry t_lc, r)

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


parseTagType :: String -> (Maybe TagType, String)
parseTagType s = let (t, r) = span Char.isAlpha s
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
                 _              -> (Just (UnknownTag t_lc), r)

parseTagValue :: String -> (TagValue, String)
parseTagValue = _parseTagValue [ numberParser
                               , referenceParser
                               , delimitedValueParser
                               ]

-- | a tuple describing a tag value parser
-- 1st element: predicate function to determine if the parser can be applied to
--              an input stream
-- 2nd element: the actual parser function which returns the raw parsed value
--              and the remaining input stream
-- 3rd element: constructor function to turn the raw value into a TagValue
type TagValueParser = ( String -> Bool
                      , String -> (String, String)
                      , String -> TagValue
                      )

numberParser :: TagValueParser
numberParser = ( \ s -> (not (null s)) && (Char.isDigit (head s))
               , span Char.isDigit
               , LiteralValue
               )

isReferencedValueChar :: Char -> Bool
isReferencedValueChar = Char.isAlpha

referenceParser :: TagValueParser
referenceParser = ( \ s -> (not (null s)) && (isReferencedValueChar (head s))
                  , span isReferencedValueChar
                  , ReferencedValue
                  )

delimitedValueParser :: TagValueParser
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
_parseTagValue :: [ TagValueParser ]
               -> String
               -> (TagValue, String)
_parseTagValue ps cs =
    let cs' = lstrip cs
    in foldr ( \ (pr, f, cons) b ->
                   if pr cs'
                       then let (v_raw, r) = f cs'
                                v = cons v_raw :: TagValue
                            in case lstrip r of
                                   '#':rs -> let (v', r') = _parseTagValue ps rs
                                             in (ComposedValue v v', r')
                                   _      -> (v, r)
                            else b
             )
             ( error_midway cs $ "_parseTagValue{BibToXml}: Found no matching "
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


parseTag :: String -> (Maybe (TagType, TagValue), String)
parseTag s = let (t, r) = parseTagType (lstrip s) in
             case t of
             -- If t is Nothing, there wasn't found any tag type. We assume that
             -- the tag is empty and therefore evaluate to Nothing and the
             -- remaining input stream.
             Nothing -> (Nothing, r)
             -- If a tag type has been found, look for an equals character
             -- (i.e. '='), optionally preceded by some whitespace. If found,
             -- start parsing the tag value. Otherwise, raise an error.
             Just t' -> case lstrip r of
                        '=':r' -> let (v, r'') = parseTagValue r' in
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

parseTags :: String -> (Tags, String)
parseTags s = let (mtv, r) = parseTag s
                  r'       = lstrip r
              in
              case r' of
              -- If we encounter a comma, then there could be another tag. Union
              -- the current tag (i.e. mtv) with all following tags.
              ',':cs -> let (m, r'') = parseTags cs in
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


parseEntry :: String -> (Element, String)
parseEntry s =
    let (t, s') = parseEntryType s
        s''     = lstrip s'
    in
    case s'' of
    '{':cs -> let (k, s) = parseEntryKey cs in
              case s of
              ',':cs -> let (m, r) = parseTags cs in
                        case lstrip r of
                        '}':r' -> (Entry t k m, r')
                        c:cs   -> error_midway
                                      (c:cs)
                                      $ "parseEntry{BibToXml}: found '"
                                        ++ [c]
                                        ++ "' when expecting the following "
                                        ++ "character: '}'"
                        ""     -> error $ "parseEntry{BibToXml}: found end of "
                                          ++ "string when expecting the "
                                          ++ "following character: '}'"
              c:cs   -> error_midway
                            (c:cs)
                            $ "parseEntry{BibToXml}: found '"
                              ++ [c]
                              ++ "' when expecting the following character: ','"
              ""     -> error $ "parseEntry{BibToXml}: found end " ++
                                "of string when expecting the " ++
                                "following character: ','"
    ""     -> error $ "parseEntry{BibToXml}: found end of string " ++
                      "when expecting the following character: '{'"
    c:cs   -> error_midway
                  (c:cs)
                  $ "parseEntry{BibToXml}: found '"
                    ++ [c]
                    ++ "' when expecting the following character: '{'"


strip_comments :: String -> String
strip_comments ""           = ""
strip_comments ('%':r)      = case dropWhile (/= '\n') r of
                                  ""   -> ""
                                  _:r' -> strip_comments r'
strip_comments ('\\':'%':r) = "\\%" ++ strip_comments r
strip_comments (c:cs)       = c : strip_comments cs

parse :: String -> Database
parse s = let s' = strip_comments $ lstrip s in
          case s' of
          []     -> emptyDatabase
          '@':cs -> let (e, r) = parseEntry cs
                        es = parse r
                    in pushFront e es
          c:cs   -> error $
                        "parseBibtex{BibToXml}: found '" ++ [c] ++
                        "' when expecting one of the following: '@'"


error_midway remaining message = error $ message
                                         ++ "\nAt:\n"
                                         ++ case take 50 remaining of
                                                "" -> "\\0"
                                                s  -> s
