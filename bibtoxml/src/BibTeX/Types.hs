module BibTeX.Types where

import Prelude
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Char as Char


data EntryType = Article | Book | Booklet | Conference | Inbook | Incollection |
                 Inproceedings | Manual | Masterthesis | Misc | Phdthesis |
                 Proceedings | Techreport | Unpublished | UnknownEntry String
                 deriving Show

data TagType = Address | Author | Booktitle | Chapter | Edition | Editor |
               Howpublished | Institution | Isbn | Journal | Month | Note |
               Number | Organization | Pages | Publisher | School | Series |
               Title | Type | Volume | Year | UnknownTag String
               deriving (Show, Eq, Ord)

type EntryKey = String
data TagValue = LiteralValue String
              | ReferencedValue String
              | ComposedValue TagValue TagValue
              deriving Show

composeTagValue :: TagValue -> TagValue -> TagValue
composeTagValue (LiteralValue s1) (LiteralValue s2) = LiteralValue (s1 ++ s2)
composeTagValue v1 v2                               = ComposedValue v1 v2

expandTagValue :: StringMap -> TagValue -> TagValue
expandTagValue _ (LiteralValue s)      = LiteralValue s
expandTagValue m (ReferencedValue s)   = case m Map.!? s of
                                             Nothing -> undefined
                                             Just v  -> expandTagValue m v
expandTagValue m (ComposedValue v1 v2) = composeTagValue (expandTagValue m v1)
                                                         (expandTagValue m v2)


type StringMap = Map String TagValue

type Tags     = Map TagType TagValue


data Element = Entry      { entryType :: EntryType
                          , entryKey  :: EntryKey
                          , entryTags :: Tags
                          }
             | Comment    String
             | StringDecl StringMap
             deriving Show

emptyEntry :: EntryType -> EntryKey -> Element
emptyEntry t k = Entry { entryType = t
                       , entryKey  = k
                       , entryTags = Map.empty
                       }

addTag :: TagType -> TagValue -> Element -> Element
addTag ttype tval (Entry etype ekey etags) =
    Entry { entryType = etype
          , entryKey  = ekey
          , entryTags = Map.insert ttype tval etags
          }


type Database = [Element]

emptyDatabase = [] :: Database


-- | /O(1)/ Append the element to the front of the database
pushFront :: Element -> Database -> Database
pushFront e db = e : db

-- | /O(n)/ Append the element to the back of the database
pushBack :: Element -> Database -> Database
pushBack e db  = db ++ [e]
