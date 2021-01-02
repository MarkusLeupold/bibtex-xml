{-# LANGUAGE FlexibleInstances #-}

module BibTeX.Output.XML where

import qualified BibTeX.Types as BT
import qualified Text.XML.Light as XML
import qualified Data.Map.Strict as Map


class ToString t where
    toString :: t -> String

instance ToString BT.EntryType where
    toString BT.Article          = "article"
    toString BT.Book             = "book"
    toString BT.Booklet          = "booklet"
    toString BT.Conference       = "conference"
    toString BT.Inbook           = "inbook"
    toString BT.Incollection     = "incollection"
    toString BT.Inproceedings    = "inproceedings"
    toString BT.Manual           = "manual"
    toString BT.Masterthesis     = "masterthesis"
    toString BT.Misc             = "misc"
    toString BT.Phdthesis        = "phdthesis"
    toString BT.Proceedings      = "proceedings"
    toString BT.Techreport       = "techreport"
    toString BT.Unpublished      = "unpublished"
    toString (BT.UnknownEntry s) = s

instance ToString BT.TagType where
    toString BT.Address        = "address"
    toString BT.Author         = "author"
    toString BT.Booktitle      = "booktitle"
    toString BT.Chapter        = "chaper"
    toString BT.Edition        = "edition"
    toString BT.Editor         = "editor"
    toString BT.Howpublished   = "howpublished"
    toString BT.Institution    = "institution"
    toString BT.Isbn           = "isbn"
    toString BT.Journal        = "journal"
    toString BT.Month          = "month"
    toString BT.Note           = "note"
    toString BT.Number         = "number"
    toString BT.Organization   = "organization"
    toString BT.Pages          = "pages"
    toString BT.Publisher      = "publisher"
    toString BT.School         = "school"
    toString BT.Series         = "series"
    toString BT.Title          = "title"
    toString BT.Type           = "type"
    toString BT.Volume         = "volume"
    toString BT.Year           = "year"
    toString (BT.UnknownTag s) = s

instance ToString String where toString s = s

class ToElement t where
    toElement :: t -> XML.Element


tagValueToElementList :: BT.TagValue -> [XML.Element]
tagValueToElementList (BT.LiteralValue s) =
    [ XML.node ( XML.blank_name { XML.qName = "literalValue" } )
               ( s )
    ]
tagValueToElementList (BT.ReferencedValue s) =
    [ XML.node ( XML.blank_name { XML.qName = "referencedValue" } )
               ( XML.Attr { XML.attrKey = XML.blank_name { XML.qName = "ref" }
                          , XML.attrVal = s
                          }
               )
    ]
tagValueToElementList (BT.ComposedValue v1 v2) =
    (tagValueToElementList v1) ++ (tagValueToElementList v2)

instance ToElement (BT.TagType, BT.TagValue) where
    toElement (t, v) =
        XML.node ( XML.blank_name { XML.qName = "tag" } )
                 ( [ XML.Attr { XML.attrKey = XML.blank_name
                                                  { XML.qName = "type" }
                              , XML.attrVal = toString t
                              }
                   ]
                 , tagValueToElementList v
                 )

instance ToElement BT.Element where
    toElement BT.Entry { BT.entryType = t
                       , BT.entryKey  = k
                       , BT.entryTags = tags
                       } =
        XML.node ( XML.blank_name { XML.qName = "entry" } )
                 ( [ XML.Attr { XML.attrKey = XML.blank_name
                                                  { XML.qName = "id" }
                              , XML.attrVal = toString k
                              }
                   , XML.Attr { XML.attrKey = XML.blank_name
                                                  { XML.qName = "type" }
                              , XML.attrVal = toString t
                              }
                   ]
                 , foldr ((:) . XML.Elem . toElement) [] $ Map.toAscList tags
                 )
    toElement (BT.Comment s) =
        XML.node ( XML.blank_name { XML.qName = "comment" } )
                 s

instance ToElement [BT.Element] where
    toElement es = XML.node ( XML.blank_name { XML.qName = "database" } )
                            ( map toElement es )
