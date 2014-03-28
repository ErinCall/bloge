{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main
    where

import           ArbitraryInstances()
import           Test.QuickCheck
import           Test.QuickCheck.All
import           Document

instance Arbitrary Document where
  arbitrary = do
    dTitle <- arbitrary
    dPosted <- arbitrary
    dTags <- arbitrary
    dSlug <- arbitrary
    dBody <- arbitrary
    return Document {..}

prop_eq_derivation_still_works x = x == x

main = $quickCheckAll
