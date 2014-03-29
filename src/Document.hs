{-# LANGUAGE RecordWildCards #-}

module Document (
    Tag,
    Document(..),
    parse,
  ) where

import           Data.Time.Clock
import           ISO8601 (parseISO8601)
import qualified Data.Text         as T
import           Text.Parsec       hiding (parse)
import qualified Text.Parsec       as P

import Document.Internal

type Tag = T.Text

data Document = Document {
    dTitle  :: T.Text
  , dSlug   :: T.Text
  , dPosted :: UTCTime
  , dTags   :: [ Tag ]
  , dBody   :: T.Text
} deriving (Eq)
instance Show Document where
  show = T.unpack . dTitle

parse :: String -> Either ParseError Document
parse = P.parse document ""
  where
    document = do
      dTitle  <- fmap T.pack title
      dSlug   <- fmap T.pack slug
      postStr <- posted
      dPosted <- case parseISO8601 postStr of
            Nothing -> unexpected "Posted date must be an ISO 8601 datetime"
            Just x  -> return x
      dTags <- fmap (map T.pack) tags
      dBody <- fmap T.pack body
      eof
      return Document {..}
