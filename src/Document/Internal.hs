{-# LANGUAGE RecordWildCards #-}

module Document.Internal where

import qualified Data.Text             as T
import           Data.Functor.Identity
import           Data.Time.Clock
--import           Data.Time.ISO8601
import           ISO8601 (parseISO8601)
import           Text.Parsec

data Field = Title T.Text
           | Slug T.Text
           | Posted UTCTime
           | Tags [T.Text]
           | Body T.Text
  deriving (Show, Eq)

title :: ParsecT String u Identity Field
title = do
  string "Title: "
  titleText <- singleLine
  return $ Title $ T.pack titleText

slug :: ParsecT String u Identity Field
slug = do
  string "Slug: "
  let slugChar = alphaNum <|> char '-' <|> char '_'
                 <?> "URL-friendly string: alphanumerics, -, or _"
  slugText <- manyTill slugChar (char '\n')
  return $ Slug $ T.pack slugText

posted :: ParsecT String u Identity Field
posted = do
  string "Posted: "
  postedAt <- singleLine
  postedDate <- case parseISO8601 postedAt of
        Just x -> return x
        Nothing -> unexpected "Posted date must be an ISO 8601 datetime"
  return $ Posted postedDate

tags :: ParsecT String u Identity Field
tags = do
    string "Tags:\n"
    tagList <- many $ (string "    ") >> singleLine
    return $ Tags $ map T.pack tagList
  <|> (return $ Tags [])

body :: ParsecT String u Identity Field
body = do
  contents <- many1 singleLine
  return $ Body $ T.pack $ unlines contents

singleLine :: ParsecT String u Identity String
singleLine = manyTill anyChar (char '\n')
