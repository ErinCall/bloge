{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Document.Internal where


import           Data.Default          (def)
import qualified Data.Set              as S
import qualified Data.Text             as T
import           Data.Functor.Identity
import qualified Data.Text.Lazy        as L
import           Data.Time.Clock
import           Data.Time.ISO8601     (parseISO8601)
import           Text.Blaze.Html       (Html)
import           Text.Markdown         (markdown, MarkdownSettings(..))
import           Text.Parsec
import           Text.Parsec.Perm

type Fields = (T.Text, T.Text, T.Text, UTCTime, [T.Text])

fields :: ParsecT String u Identity Fields
fields  = permute (tuple <$$> title
                         <|?> ("", slug)
                         <|?> ("", disqusId)
                         <||> posted
                         <|?> ([], tags))
      where
        tuple a b c d e = (a, b, c, d, e)

title :: ParsecT String u Identity T.Text
title = fmap T.pack $ try $ (string "Title: ") >> singleLine

slug :: ParsecT String u Identity T.Text
slug = try $ do
  string "Slug: "
  let slugChar = alphaNum <|> char '-' <|> char '_'
                 <?> "URL-friendly string: alphanumerics, -, or _"
  fmap T.pack $ manyTill slugChar (char '\n')

disqusId :: ParsecT String u Identity T.Text
disqusId = fmap T.pack $ try $ (string "DisqusId: ") >> singleLine

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

aboveFold :: ParsecT String u Identity Html
aboveFold = do
    let dashes = many1 $ char '-'
        separator = dashes >> string "8<" >> dashes >> char '\n'
    above <- manyTill singleLine $ try separator
    return $ md $ L.pack $ unlines above

belowFold :: ParsecT String u Identity Html
belowFold = fmap (md . L.pack . unlines) $ many1 singleLine

singleLine :: ParsecT String u Identity String
singleLine = manyTill anyChar (char '\n')

slugify :: T.Text -> T.Text
slugify = T.filter (`S.member` legalChars) . T.map despace . T.toLower
  where despace ' ' = '-'
        despace x = x
        legalChars = S.fromList $ '-' : ['0'..'9'] ++ "_" ++ ['a'..'z']

md :: L.Text -> Html
md = markdown $ def { msXssProtect = False }
