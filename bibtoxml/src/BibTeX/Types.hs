module BibTeX.Types where

import Prelude
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Char as Char


data EntryType = Article | Book | Booklet | Conference | Inbook | Incollection |
                 Inproceedings | Manual | Masterthesis | Misc | Phdthesis |
                 Proceedings | Techreport | Unpublished | UnknownEntry String
                 deriving Show

data TagType = Address | Author | Booktitle | Chapter | Editor | Institution |
               Journal | Note | Pages | Publisher | School | Title | Year |
               UnknownTag String
               deriving (Show, Eq, Ord)

data Entry = Entry EntryType String (Map TagType String) deriving Show

newEntry :: EntryType -> String -> Entry
newEntry t k = Entry t k Map.empty

addTag :: TagType -> String -> Entry -> Entry
addTag k v (Entry n t tags) = Entry n t (Map.insert k v tags)

