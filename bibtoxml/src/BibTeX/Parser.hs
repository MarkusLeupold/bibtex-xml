module BibTeX.Parser (parse) where

import BibTeX.Types
import Prelude
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Char as Char
import Util


parseEntryType :: String -> (EntryType, String)
parseEntryType s = let (t, r) = span Char.isAlpha s
                       t_lc   = map Char.toLower t
                   in
                   case t_lc of
                   "" -> error $ "parseEntryType{BibToXml}: Missing entry " ++
                                 "type. Expecting at least one alphabetic " ++
                                 "character."
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
isEntryKeyLaterChar c = Char.isAlphaNum c || elem c ['-',':']

parseEntryKey :: String -> (String, String)
parseEntryKey s = case lstrip s of
                  ""   -> error $ "parseEntryKey{BibToXml}: found end of " ++
                                  "string when expecting an alphabetic " ++
                                  "character"
                  c:cs -> if isEntryKeyFirstChar c
                          then let (p, r) = span isEntryKeyLaterChar cs
                               in (c:p, r)
                          else error $ "parseEntryKey{BibToXml}: found '" ++
                                       [c] ++ "' when expecting an " ++
                                       "alphabetic character"


parseTagType :: String -> (Maybe TagType, String)
parseTagType s = let (t, r) = span Char.isAlpha s
                     t_lc   = map Char.toLower t
                 in
                 case t_lc of
                 ""            -> (Nothing, r)
                 "address"     -> (Just Address, r)
                 "author"      -> (Just Author, r)
                 "booktitle"   -> (Just Booktitle, r)
                 "chaper"      -> (Just Chapter, r)
                 "editor"      -> (Just Editor, r)
                 "institution" -> (Just Institution, r)
                 "journal"     -> (Just Journal, r)
                 "note"        -> (Just Note, r)
                 "pages"       -> (Just Pages, r)
                 "publisher"   -> (Just Publisher, r)
                 "school"      -> (Just School, r)
                 "title"       -> (Just Title, r)
                 "year"        -> (Just Year, r)
                 _             -> (Just (UnknownTag t_lc), r)

parseTagValue :: String -> (String, String)
parseTagValue s = case lstrip s of

    '"':cs -> let (v, r) = _parseTagValueQuoted cs in
              case r of
              '"':r' -> (v, r')
              ""     -> error $ "parseTagValue{BibToXml}: found end of " ++
                                "string when expecting the following " ++
                                "character: '\"'"
              r1:r'  -> error $ "parseTagValue{BibToXml}: found '" ++ [r1] ++
                                "' when expecting the following character: '\"'"

    '{':cs -> let (v, r) = _parseTagValueBraced cs in
              case r of
              '}':r' -> (v, r')
              ""     -> error $ "parseTagValue{BibToXml}: found end of "++
                                "string when expecting the following " ++
                                "character: '}'"
              r1:r'  -> error $ "parseTagValue{BibToXml}: found '" ++ [r1] ++
                                "' when expecting the following character: '}'"

    c:cs   -> if Char.isDigit c 
              then let (v, r) = span Char.isDigit cs in
                   (c:v, r)
              else error $ "parseTagValue{BibToXml}: found '" ++ [c] ++
                           "' when expecting a digit or one of the " ++
                           "following characters: '\"', '{'"

    ""     -> error $ "parseTagValue{BibToXml}: found end of string when " ++
                      "expecting a digit or one of the following " ++
                      "characters: '\"', '{'"


_parseTagValueQuoted :: String -> (String, String)
_parseTagValueQuoted ""          = ("", "")
-- A backslash (i.e. the escape character) lets the next character be
-- interpreted by TeX, so we aren't allowed to interpret it as a BibTeX control
-- character. Therefore, we copy it to the result without even looking at it.
_parseTagValueQuoted ('\\':c:cs) = let (v, r) = _parseTagValueQuoted cs
                                   in ('\\':c:v, r)
_parseTagValueQuoted ('"':cs)    = ("", '"':cs)  -- end of value; put '"' back
_parseTagValueQuoted (c:cs)      = let (v, r) = _parseTagValueQuoted cs
                                   in (c:v, r)

_parseTagValueBraced :: String -> (String, String)
_parseTagValueBraced = __parseTagValueBraced 0

-- | Value parser for values inside curly braces. The first argument is the
-- | brace count inside the value. The surrounding delimiting braces of BibTeX
-- | aren't counted by this argument.
__parseTagValueBraced :: Int -> String -> (String, String)
__parseTagValueBraced _ ""          = ("", "")

-- Increment the brace count when encountering an opening curly brace (i.e. '{')
-- and continue parsing.
__parseTagValueBraced b ('{':cs)    = let (v, r) = __parseTagValueBraced
                                                   (b+1)
                                                   cs
                                      in ('{':v, r)

-- While the brace count is zero, unescaped closing curly braces (i.e. '}') are
-- not allowed to occur inside the value. This means, that if we encounter one
-- in this state we must have reached the end of the value. We put the brace
-- back into the input stream and return the empty string (i.e. the result
-- value) together with the remaining input stream.
__parseTagValueBraced 0 ('}':cs)    = ("", '}':cs)  -- put '}' back

-- Decrement the brace count when encountering a closing curly brace (i.e. '}')
__parseTagValueBraced b ('}':cs)    = let (v, r) = __parseTagValueBraced
                                                   (b-1)
                                                   cs
                                      in ('}':v, r)
                                      
-- A backslash (i.e. the TeX escape character) introduces a TeX control
-- sequence. These are not touched by BibTeX, so we aren't allowed to interpret
-- the following character as a BibTeX control character. In the special case
-- that the following character is a curly brace, TeX won't interpret it as a
-- block beginning or end, because it is escaped.
-- Therefore, we can copy the character after the backslash to the result
-- without even looking at it and we are sure that we don't need to change the
-- brace count.
__parseTagValueBraced b ('\\':c:cs) = let (v, r) = __parseTagValueBraced b cs
                                      in ('\\':c:v, r)

-- When none of the special cases above match the input, simply continue
-- parsing.
__parseTagValueBraced b (c:cs)      = let (v, r) = __parseTagValueBraced b cs
                                      in (c:v, r)


parseTag :: String -> (Maybe (TagType, String), String)
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
                        c:_    -> error $ "parseTag{BibToXml}: found '" ++
                                          [c] ++ "' when expecting the " ++
                                          "following character: '='"
                        ""     -> error $ "parseTag{BibToXml}: found " ++
                                          "end of string when expecting " ++
                                          "the following character: '='"

parseTags :: String -> ((Map TagType String), String)
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
                   

parseEntry :: String -> (Entry, String)
parseEntry s = let (t, s') = parseEntryType s
                   s''     = lstrip s'
               in
               case s'' of
               '{':cs -> let (k, s) = parseEntryKey cs in
                         case s of
                         ',':cs -> let (m, r) = parseTags cs in
                                   case lstrip r of
                                   '}':r' -> (Entry t k m, r')
                                   c:_    -> error $
                                             "parseEntry{BibToXml}: found '" ++
                                             [c] ++ "' when expecting the " ++
                                             "following character: '}'"
                                   ""     -> error $
                                             "parseEntry{BibToXml}: found " ++
                                             "end of string when expecting " ++
                                             "the following character: '}'"
                         c:_    -> error $ "parseEntry{BibToXml}: found '" ++
                                           [c] ++ "' when expecting the " ++
                                           "following character: ','"
                         ""     -> error $ "parseEntry{BibToXml}: found end " ++
                                           "of string when expecting the " ++
                                           "following character: ','"
               ""     -> error $ "parseEntry{BibToXml}: found end of string " ++
                                 "when expecting the following character: '{'"
               c:_    -> error $ "parseEntry{BibToXml}: found '" ++ [c] ++
                                 "' when expecting the following character: '{'"
                   

parse :: String -> ([Entry], String)
parse s = let s' = lstrip s in
          case s' of
          []     -> ([], "")
          '@':cs -> let (e, r) = parseEntry cs
                        (es, q) = parse r
                    in  (e:es, q)
          c:cs   -> error $
                        "parseBibtex{BibToXml}: found '" ++ [c] ++
                        "' when expecting one of the following: '@'"

