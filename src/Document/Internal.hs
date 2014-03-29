{-# LANGUAGE RecordWildCards #-}

module Document.Internal where

import           Data.Functor.Identity
import           Data.Time.Clock
--import           Data.Time.ISO8601
import           ISO8601 (parseISO8601)
import           Text.Parsec

title :: ParsecT String u Identity String
title = (string "Title: ") >> singleLine

slug :: ParsecT String u Identity String
slug = do
  string "Slug: "
  let slugChar = alphaNum <|> char '-' <|> char '_'
                 <?> "URL-friendly string: alphanumerics, -, or _"
  manyTill slugChar (char '\n')

posted :: ParsecT String u Identity UTCTime
posted = do
  string "Posted: "
  postedAt <- singleLine
  case parseISO8601 postedAt of
        Just x -> return x
        Nothing -> unexpected "Posted date must be an ISO 8601 datetime"

tags :: ParsecT String u Identity [String]
tags = do
  string "Tags:\n"
  many $ (string "    ") >> singleLine

body :: ParsecT String u Identity String
body = fmap unlines $ many1 singleLine

singleLine :: ParsecT String u Identity String
singleLine = manyTill anyChar (char '\n')
