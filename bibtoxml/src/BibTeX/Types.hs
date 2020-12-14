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
type TagValue = String
type Tags     = Map TagType TagValue

data Entry = Entry { entryType :: EntryType
                   , entryKey  :: EntryKey
                   , entryTags :: Tags
                   } deriving Show

type Database = [Entry]

emptyEntry :: EntryType -> EntryKey -> Entry
emptyEntry t k = Entry { entryType = t
                       , entryKey  = k
                       , entryTags = Map.empty
                       }

addTag :: TagType -> TagValue -> Entry -> Entry
addTag k v e = e { entryTags = Map.insert k v (entryTags e) }

