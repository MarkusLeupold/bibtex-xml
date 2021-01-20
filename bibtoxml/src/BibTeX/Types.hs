module BibTeX.Types where

import Prelude
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Char as Char


data EntryType = Article | Book | Booklet | Conference | Inbook | Incollection |
                 Inproceedings | Manual | Masterthesis | Misc | Phdthesis |
                 Proceedings | Techreport | Unpublished | UnknownEntry String
                 deriving Show

data FieldName = Address | Author | Booktitle | Chapter | Edition | Editor |
                 Howpublished | Institution | Isbn | Journal | Month | Note |
                 Number | Organization | Pages | Publisher | School | Series |
                 Title | Type | Volume | Year | UnknownField String
                 deriving (Show, Eq, Ord)

type EntryKey = String
data Value = LiteralValue String
           | ReferencedValue String
           | ComposedValue Value Value
           deriving Show

composeValue :: Value -> Value -> Value
composeValue (LiteralValue s1) (LiteralValue s2) = LiteralValue (s1 ++ s2)
composeValue v1 v2                               = ComposedValue v1 v2

expandValue :: StringMap -> Value -> Value
expandValue _ (LiteralValue s)      = LiteralValue s
expandValue m (ReferencedValue s)   = case m Map.!? s of
                                          Nothing -> undefined
                                          Just v  -> expandValue m v
expandValue m (ComposedValue v1 v2) = composeValue (expandValue m v1)
                                                   (expandValue m v2)


type StringMap = Map String Value

type Fields    = Map FieldName Value


data Element = Entry      { entryType   :: EntryType
                          , entryKey    :: EntryKey
                          , entryFields :: Fields
                          }
             | Comment    String
             | StringDecl StringMap
             deriving Show

emptyEntry :: EntryType -> EntryKey -> Element
emptyEntry t k = Entry { entryType   = t
                       , entryKey    = k
                       , entryFields = Map.empty
                       }

addTag :: FieldName -> Value -> Element -> Element
addTag ttype tval (Entry etype ekey eFields) =
    Entry { entryType   = etype
          , entryKey    = ekey
          , entryFields = Map.insert ttype tval eFields
          }


type Database = [Element]

emptyDatabase = [] :: Database


-- | /O(1)/ Append the element to the front of the database
pushFront :: Element -> Database -> Database
pushFront e db = e : db

-- | /O(n)/ Append the element to the back of the database
pushBack :: Element -> Database -> Database
pushBack e db  = db ++ [e]
