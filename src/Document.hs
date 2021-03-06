{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Document (
    Tag,
    Document(..),
    insertDoc,
    parse,
    parseFile,
  ) where

import           Data.HashMap                  (Map, alter)
import           Data.Maybe                    (fromMaybe)
import           Data.Time.Clock
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as L
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Parsec                   hiding (parse)
import qualified Text.Parsec                   as P

import Document.Internal

type Tag = T.Text

instance Eq Html where
  a == b = (renderHtml a == renderHtml b)

data Document = Document {
    dTitle     :: T.Text
  , dSlug      :: T.Text
  , dDisqusId  :: T.Text
  , dPosted    :: UTCTime
  , dTags      :: [ Tag ]
  , dAboveFold :: Html
  , dBelowFold :: Html
  , dHasFold   :: Bool
} deriving (Eq)
instance Show Document where
  show d = T.unpack $ T.unlines $ map ($ d)
           [dTitle, dSlug, above, below]
    where
      above = T.append "Above:\n" . render . dAboveFold
      below = T.append "Below:\n" . render . dBelowFold
      render = L.toStrict . renderHtml

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
      (dTitle, mSlug, mDisqusId, dPosted, dTags) <- fields
      let dSlug = if T.null mSlug
                  then slugify dTitle
                  else mSlug
          dDisqusId = if T.null mDisqusId
                      then dSlug
                      else mDisqusId

      dAboveFold <- try aboveFold <|> belowFold
      dBelowFold <- try belowFold <|> (return "")
      eof

      let dHasFold = not $ L.null $ renderHtml dBelowFold

      return Document {..}

type DocMap = Map T.Text [Document]

insertDoc :: Document -> DocMap -> DocMap
insertDoc d h = foldr insertMe h $ dTags d
  where
    insertMe tag hash = alter (\l -> Just $ d : fromMaybe [] l) tag hash
