{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Document.Internal where

import qualified Data.Set              as S
import qualified Data.Text             as T
import           Data.Functor.Identity
import           Data.Time.Clock
--import           Data.Time.ISO8601
import           ISO8601               (parseISO8601)
import           Text.Parsec
import           Text.Parsec.Perm

type Fields = (T.Text, T.Text, UTCTime, [T.Text])

fields :: ParsecT String u Identity Fields
fields  = permute (tuple <$$> title
                         <|?> ("", slug)
                         <||> posted
                         <|?> ([], tags))
      where
        tuple a b c d = (a, b, c, d)

title :: ParsecT String u Identity T.Text
title = fmap T.pack $ try $ (string "Title: ") >> singleLine

slug :: ParsecT String u Identity T.Text
slug = try $ do
  string "Slug: "
  let slugChar = alphaNum <|> char '-' <|> char '_'
                 <?> "URL-friendly string: alphanumerics, -, or _"
  fmap T.pack $ manyTill slugChar (char '\n')

posted :: ParsecT String u Identity UTCTime
posted = try $ do
  string "Posted: "
  postedAt <- singleLine
  case parseISO8601 postedAt of
        Just x -> return x
        Nothing -> unexpected "Posted date must be an ISO 8601 datetime"

tags :: ParsecT String u Identity [T.Text]
tags = try $ do
  string "Tags:\n"
  fmap (map T.pack) $ many $ (string "    ") >> singleLine

body :: ParsecT String u Identity T.Text
body = fmap T.pack $ fmap unlines $ many1 singleLine

singleLine :: ParsecT String u Identity String
singleLine = manyTill anyChar (char '\n')

slugify :: T.Text -> T.Text
slugify = T.filter (`S.member` legalChars) . T.map despace . T.toLower
  where despace ' ' = '-'
        despace x = x
        legalChars = S.fromList $ '-' : ['0'..'9'] ++ "_" ++ ['a'..'z']
