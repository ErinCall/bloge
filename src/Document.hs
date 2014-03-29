{-# LANGUAGE RecordWildCards #-}

module Document (
    Tag,
    Document(..),
    parse,
  ) where

import           Data.Time.Clock
import           Data.List
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
      fieldList <- foldl1 (<|>) $ map (getFields . (map try)) $
                        permutations [title, slug, posted, tags]
      dBody <- body
      eof
      return $ fromList (dBody: fieldList)
    getFields (p:ps) = do
      field <- p
      furtherFields <- getFields ps
      return (field:furtherFields)
    getFields [] = return []

fromList :: [Field] -> Document
fromList = fromList' (Nothing, Nothing, Nothing, Nothing, Nothing)
  where
    fromList' ((Just dTitle),
               (Just dSlug),
               (Just dPosted),
               (Just dTags),
               (Just dBody)) _ = Document{..}
    fromList' (_, s, p, ts, b) ((Title t):fs)  = fromList' (Just t, s, p, ts, b) fs
    fromList' (t, _, p, ts, b) ((Slug s):fs)   = fromList' (t, Just s, p, ts, b) fs
    fromList' (t, s, _, ts, b) ((Posted p):fs) = fromList' (t, s, Just p, ts, b) fs
    fromList' (t, s, p, _,  b) ((Tags ts):fs)  = fromList' (t, s, p, Just ts, b) fs
    fromList' (t, s, p, ts, _) ((Body b):fs)   = fromList' (t, s, p, ts, Just b) fs
    fromList' _ [] = undefined

