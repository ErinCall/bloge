module ISO8601 where

import           Data.Time.Clock
import           Control.Applicative ((<|>))
import           Data.Time.Format (parseTime)
import           System.Locale (defaultTimeLocale)

--this function's grabbed from Data.Time.ISO8601;
--it's inlined here until https://github.com/nh2/iso8601-time/pull/2
--is on hackage
parseISO8601 :: String -> Maybe UTCTime
parseISO8601 t = parseTime defaultTimeLocale "%FT%T%QZ" t <|>
                 parseTime defaultTimeLocale "%FT%T%Q%z" t
