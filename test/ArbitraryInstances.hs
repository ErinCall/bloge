{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ArbitraryInstances
    where

import           Test.QuickCheck
import           Data.Default       (def)
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as L
import           Text.Blaze.Html    (Html)
import           Text.Markdown      (markdown)
import           Document

instance Arbitrary Day where
  arbitrary = do
    toModifiedJulianDay <- arbitrary
    return ModifiedJulianDay {..}

instance Arbitrary UTCTime where
  arbitrary = do
    utctDay <- arbitrary
    utctDayTime <- arbitrary
    return UTCTime {..}

instance Arbitrary DiffTime where
  arbitrary = arbitrary >>= (return . secondsToDiffTime)

instance Arbitrary T.Text where
  arbitrary = arbitrary >>= (return . T.pack)

instance Arbitrary Html where
  arbitrary = arbitrary >>= (return . (markdown def) . L.pack)

instance Arbitrary Document where
  arbitrary = do
    dTitle <- arbitrary
    dPosted <- arbitrary
    dDisqusId <- arbitrary
    dTags <- arbitrary
    dSlug <- arbitrary
    dAboveFold <- arbitrary
    dBelowFold <- arbitrary
    dHasFold <- arbitrary
    return Document {..}
