{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import ArbitraryInstances()
import Document

import TestDocumentParsing as DP
import TestTagList         as TL

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "Document properties" [
      testProperty "eq derivation" prop_eq_derivation_still_works
    ]
  ] ++ DP.testGroups
    ++ TL.testGroups

prop_eq_derivation_still_works :: Document -> Bool
prop_eq_derivation_still_works x = x == x

