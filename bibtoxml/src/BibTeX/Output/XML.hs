{-# LANGUAGE FlexibleInstances #-}

module BibTeX.Output.XML ( Format(..)
                         , toElement
                         , toElementWithoutComments
                         ) where

import qualified BibTeX.Types as BT
import qualified Text.XML.Light as XML
import qualified Data.Map.Strict as Map


data Format = Format { includeComments :: Bool
                     , linkXMLSchema   :: Maybe String
                     } deriving Show

bibtex_xml_ns_uri = "https://github.com/MarkusLeupold/bibtex-xml"
xml_schema_instance = "http://www.w3.org/2001/XMLSchema-instance"


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
    toString BT.Mastersthesis    = "mastersthesis"
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
    toString BT.Chapter        = "chapter"
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


name_with_ns = XML.blank_name { XML.qURI = Just bibtex_xml_ns_uri } :: XML.QName


class ToElements t where
    toElements :: Format -> t -> [XML.Element]


instance ToElements BT.Value where
    toElements _ (BT.LiteralValue s) =
        [ XML.node ( name_with_ns { XML.qName = "literalValue" } )
                   ( s )
        ]
    toElements _ (BT.ReferencedValue s) =
        [ XML.node ( name_with_ns { XML.qName = "referencedValue" } )
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
        [ XML.node ( name_with_ns { XML.qName = "field" } )
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
        [ XML.node ( name_with_ns { XML.qName = "string" } )
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
        [ XML.node ( name_with_ns { XML.qName = "entry" } )
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
            [ XML.node ( name_with_ns { XML.qName = "comment" } )
                       s
            ]
        else []
    toElements f (BT.StringDecl m) = toElements f $ Map.toAscList m


attr_xmlns =
    XML.Attr
        { XML.attrKey = XML.blank_name { XML.qName = "xmlns" }
        , XML.attrVal = bibtex_xml_ns_uri
        }

attr_schema_instance =
    XML.Attr
        { XML.attrKey = XML.blank_name { XML.qName = "xmlns:xsi" }
        , XML.attrVal = xml_schema_instance
        }

attr_schema_location l =
    XML.Attr
        { XML.attrKey = XML.blank_name { XML.qName = "xsi:schemaLocation" }
        , XML.attrVal = bibtex_xml_ns_uri ++ " " ++ l
        }

instance ToElements [BT.Element] where
    toElements f es =
        [ XML.node ( name_with_ns { XML.qName = "database" } )
                   ( attr_xmlns
                     : ( maybe []
                               ( \ l -> [ attr_schema_instance
                                        , attr_schema_location l
                                        ]
                               )
                               (linkXMLSchema f)
                       )
                   , es >>= (toElements f)
                   )
        ]

toElement :: Format -> BT.Database -> XML.Element
toElement f db = head $ toElements f db

toElementWithoutComments :: BT.Database -> XML.Element
toElementWithoutComments db = toElement Format { includeComments = False
                                               , linkXMLSchema   = Nothing
                                               }
                                        db
