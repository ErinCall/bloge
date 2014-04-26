{-# LANGUAGE RecordWildCards #-}

module Document (
    Tag,
    Document(..),
    parse,
    parseFile,
  ) where

import           Data.Time.Clock
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
  show d = T.unpack $ T.unlines $ map ($ d) [dTitle, dSlug, dBody]

parse :: String -> Either ParseError Document
parse = parse' ""

parseFile :: FilePath -> IO (Either ParseError Document)
parseFile path = do
  contents <- readFile path
  return $ parse' (show path) contents

parse' :: String -> String -> Either ParseError Document
parse' filename = P.parse document filename
  where
    document = do
      (dTitle, mSlug, dPosted, dTags) <- fields
      let dSlug = if T.null mSlug
                  then slugify dTitle
                  else mSlug
      dBody <- body
      eof
      return Document {..}
