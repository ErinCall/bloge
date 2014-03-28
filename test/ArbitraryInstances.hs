{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ArbitraryInstances
    where

import           Test.QuickCheck
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Data.Text             as T

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
