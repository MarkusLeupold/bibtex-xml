{-# LANGUAGE FlexibleInstances #-}

module BibTeX.Output.XML where

import qualified BibTeX.Types as BT
import qualified Text.XML.Light as XML
import qualified Data.Map.Strict as Map


data Format = Format { includeComments :: Bool } deriving Show


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

instance ToString BT.FieldName where
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
    toString (BT.UnknownField s) = s

instance ToString String where toString s = s

class ToElements t where
    toElements :: Format -> t -> [XML.Element]


instance ToElements BT.Value where
    toElements _ (BT.LiteralValue s) =
        [ XML.node ( XML.blank_name { XML.qName = "literalValue" } )
                   ( s )
        ]
    toElements _ (BT.ReferencedValue s) =
        [ XML.node ( XML.blank_name { XML.qName = "referencedValue" } )
                   ( XML.Attr { XML.attrKey =
                                    XML.blank_name { XML.qName = "name" }
                              , XML.attrVal = s
                              }
                   )
        ]
    toElements f (BT.ComposedValue v1 v2) =
        (toElements f v1) ++ (toElements f v2)

instance ToElements (BT.FieldName, BT.Value) where
    toElements f (t, v) =
        [ XML.node ( XML.blank_name { XML.qName = "field" } )
                   ( [ XML.Attr { XML.attrKey = XML.blank_name
                                                    { XML.qName = "name" }
                                , XML.attrVal = toString t
                                }
                     ]
                   , toElements f v
                   )
        ]

instance ToElements [(BT.FieldName, BT.Value)] where
    toElements f = (>>= (toElements f))

instance ToElements (String, BT.Value) where
    toElements f (s, v) =
        [ XML.node ( XML.blank_name { XML.qName = "string" } )
                  ( [ XML.Attr { XML.attrKey = XML.blank_name
                                                   { XML.qName = "name" }
                               , XML.attrVal = s
                               }
                    ]
                  , toElements f v
                  )
         ]

instance ToElements [(String, BT.Value)] where
    toElements f = (>>= (toElements f))

instance ToElements BT.Element where
    toElements f BT.Entry { BT.entryType = t
                          , BT.entryKey  = k
                          , BT.entryFields = fields
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
                   , toElements f $ Map.toAscList fields
                   )
        ]
    toElements f (BT.Comment s) =
        if includeComments f then
            [ XML.node ( XML.blank_name { XML.qName = "comment" } )
                       s
            ]
        else []
    toElements f (BT.StringDecl m) = toElements f $ Map.toAscList m

instance ToElements [BT.Element] where
    toElements f es = [ XML.node ( XML.blank_name { XML.qName = "database" } )
                                 ( es >>= (toElements f) )
                      ]

toElement :: BT.Database -> XML.Element
toElement db =
    (\ (x:xs) -> x)
    $ toElements (Format { includeComments = True }) db

toElementWithoutComments :: BT.Database -> XML.Element
toElementWithoutComments db =
    (\ (x:xs) -> x)
    $ toElements (Format { includeComments = False }) db
