{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Document where

import qualified Data.Text         as T
import           Data.Time.Clock
--import           Data.Time.ISO8601
import           ISO8601 (parseISO8601)
import           Text.Parsec       hiding (parse)
import qualified Text.Parsec       as P

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

title = (string "Title: ") >> singleLine

slug = do
  string "Slug: "
  manyTill (alphaNum <|> char '-' <|> char '_') (char '\n')

posted = (string "Posted: ") >> singleLine

tags = do
  string "Tags:\n"
  many $ (string "    ") >> singleLine

body = fmap unlines $ many1 singleLine

singleLine = manyTill anyChar (char '\n')

parse :: String -> Either ParseError Document
parse = P.parse document ""
