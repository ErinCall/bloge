{-# LANGUAGE OverloadedStrings #-}

module Document where

import Data.Text
import Data.Time.Clock

type Tag = Text

data Document = Document {
    dTitle  :: Text
  , dPosted :: UTCTime
  , dTags   :: [ Tag ]
  , dSlug   :: Text
  , dBody   :: Text
} deriving (Eq)
