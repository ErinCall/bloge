{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TestTagList (testGroups) where

import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Data.HashMap                   (empty, fromList)
import           Data.Maybe                     (fromJust)
import           Data.Time.ISO8601              (parseISO8601)
import           Document

testGroups = [
    testGroup "Append documents to a map"
      [ testCase "a document with no tags" test_add_tagless_doc
      , testCase "disjoint tags and documents" test_disjoint_tags_and_docs
      , testCase "two documents on the same tag" test_overlapping_tags
      ]
  ]

test_add_tagless_doc = do
    let tagmap = insertDoc blankDoc empty

    tagmap @?= empty

test_disjoint_tags_and_docs = do
  let futurama = blankDoc { dTitle = "Futurama", dTags = ["sci-fi"] }
      breakingBad = blankDoc {dTitle = "Breaking Bad", dTags = ["drama"]}
      tagmap = insertDoc breakingBad $ insertDoc futurama empty

  tagmap @?= fromList [("sci-fi", [futurama]), ("drama", [breakingBad])]

test_overlapping_tags = do
  let futurama = blankDoc { dTitle = "Futurama", dTags = ["comedy"]}
      theSimpsons = blankDoc { dTitle = "The Simpsons", dTags = ["comedy"]}
      tagmap = insertDoc theSimpsons $ insertDoc futurama empty

  tagmap @?= fromList [("comedy", [theSimpsons, futurama])]

blankDoc = Document
  { dTitle = ""
  , dSlug = ""
  , dPosted = (fromJust $ parseISO8601 "2014-03-28T13:50:30Z")
  , dTags = []
  , dBody = ""
  }
