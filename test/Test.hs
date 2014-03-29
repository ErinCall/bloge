{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main
    where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
--import Test.QuickCheck
import Test.HUnit
import ArbitraryInstances()

import Data.List
import Data.Maybe
import Data.Time.ISO8601
import Document
import Text.Parsec (ParseError)

main = defaultMain tests

tests = [
    testGroup "Document properties" [
        testProperty "eq derivation" prop_eq_derivation_still_works
    ],
    testGroup "Document parsing" [
        testCase "parse a simple document" test_parse_simple_doc
    ]
  ]

prop_eq_derivation_still_works x = x == x
  where types = (x :: Document)

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
