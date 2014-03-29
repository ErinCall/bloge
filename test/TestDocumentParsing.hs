{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TestDocumentParsing (testGroups) where

import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Data.List                      (intercalate)
import           Data.Maybe                     (fromJust)
import           Data.Time.ISO8601              (parseISO8601)
import           Document
import           Document.Internal              (posted)
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Error              as P
import qualified Text.Parsec.Pos                as P

testGroups = [
    testGroup "Successful document parsing" [
      testCase "parse a simple document" test_parse_simple_doc
    ],
    testGroup "Field parsing errors" [
      testCase "date-time validation" test_fail_datetime_validation
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

test_fail_datetime_validation = do
  let result = P.parse posted "" "Posted: March 15\n"
  result @?= err "Posted date must be an ISO 8601 datetime"
    where
      err msg = Left $
          P.newErrorMessage (P.UnExpect msg) (P.newPos "" 0 0)

instance Eq P.ParseError where
  a == b = P.errorMessages a == P.errorMessages b
