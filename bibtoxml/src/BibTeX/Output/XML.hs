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

class ToElements t where
    toElements :: t -> [XML.Element]


instance ToElements BT.TagValue where
    toElements (BT.LiteralValue s) =
        [ XML.node ( XML.blank_name { XML.qName = "literalValue" } )
                   ( s )
        ]
    toElements (BT.ReferencedValue s) =
        [ XML.node ( XML.blank_name { XML.qName = "referencedValue" } )
                   ( XML.Attr { XML.attrKey =
                                    XML.blank_name { XML.qName = "ref" }
                              , XML.attrVal = s
                              }
                   )
        ]
    toElements (BT.ComposedValue v1 v2) = (toElements v1) ++ (toElements v2)

instance ToElements (BT.TagType, BT.TagValue) where
    toElements (t, v) =
        [ XML.node ( XML.blank_name { XML.qName = "tag" } )
                   ( [ XML.Attr { XML.attrKey = XML.blank_name
                                                    { XML.qName = "type" }
                                , XML.attrVal = toString t
                                }
                     ]
                   , toElements v
                   )
        ]

instance ToElements [(BT.TagType, BT.TagValue)] where
    toElements = (>>= (toElements))

instance ToElements (String, BT.TagValue) where
    toElements (s, v) =
        [ XML.node ( XML.blank_name { XML.qName = "string" } )
                  ( [ XML.Attr { XML.attrKey = XML.blank_name
                                                   { XML.qName = "name" }
                               , XML.attrVal = s
                               }
                    ]
                  , toElements v
                  )
         ]

instance ToElements [(String, BT.TagValue)] where
    toElements = (>>= (toElements))

instance ToElements BT.Element where
    toElements BT.Entry { BT.entryType = t
                        , BT.entryKey  = k
                        , BT.entryTags = tags
                        } =
        [ XML.node ( XML.blank_name { XML.qName = "entry" } )
                   ( [ XML.Attr { XML.attrKey = XML.blank_name
                                                    { XML.qName = "id" }
                                , XML.attrVal = toString k
                                }
                     , XML.Attr { XML.attrKey = XML.blank_name
                                                    { XML.qName = "type" }
                                , XML.attrVal = toString t
                                }
                     ]
                   , toElements $ Map.toAscList tags
                   )
        ]
    toElements (BT.Comment s) =
        [ XML.node ( XML.blank_name { XML.qName = "comment" } )
                   s
        ]
    toElements (BT.StringDecl m) = toElements $ Map.toAscList m

instance ToElements [BT.Element] where
    toElements es = [ XML.node ( XML.blank_name { XML.qName = "database" } )
                               ( es >>= toElements )
                    ]

toElement :: [BT.Element] -> XML.Element
toElement = (\ (x:xs) -> x) . toElements
