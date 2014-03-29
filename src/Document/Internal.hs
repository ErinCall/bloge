{-# LANGUAGE RecordWildCards #-}

module Document.Internal where

import           Data.Functor.Identity
import qualified Data.Text         as T
import           Data.Time.Clock
--import           Data.Time.ISO8601
import           ISO8601 (parseISO8601)
import           Text.Parsec       hiding (parse)

title :: ParsecT String u Identity String
title = (string "Title: ") >> singleLine

slug :: ParsecT String u Identity String
slug = do
  string "Slug: "
  manyTill (alphaNum <|> char '-' <|> char '_') (char '\n')

posted :: ParsecT String u Identity String
posted = (string "Posted: ") >> singleLine

tags :: ParsecT String u Identity [String]
tags = do
  string "Tags:\n"
  many $ (string "    ") >> singleLine

body :: ParsecT String u Identity String
body = fmap unlines $ many1 singleLine

singleLine :: ParsecT String u Identity String
singleLine = manyTill anyChar (char '\n')
