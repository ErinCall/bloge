{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TestDocumentParsing (testGroups) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Time.ISO8601 (parseISO8601)
import Document
import Text.Parsec (ParseError)

testGroups = [
    testGroup "Document parsing" [
        testCase "parse a simple document" test_parse_simple_doc
    ]
  ]

test_parse_simple_doc = do
    let parseResult = parse $ intercalate "\n" [
            "Title: Whoa Mama!"
          , "Slug: whoa-mama"
          , "Posted: 2014-03-28T06:50:30-0700"
          , "Tags:"
          , "    partying"
          , "    drinkin'"
          , "    socializing"
          , "Party at my place!"
          , "We will have fun."
          , ""
          ]
        doc = Document {
            dTitle = "Whoa Mama!"
          , dSlug = "whoa-mama"
          , dPosted = (fromJust $ parseISO8601 "2014-03-28T13:50:30Z")
          , dTags = ["partying", "drinkin'", "socializing"]
          , dBody = "Party at my place!\nWe will have fun.\n"
        }

    parseResult @?= (Right doc)

instance Eq ParseError where
  _ == _ = False
