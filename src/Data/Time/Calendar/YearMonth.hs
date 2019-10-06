{-|
Module      : Data.Time.Calendar.YearMonth
Description : Type representing months in the Gregorian calender and associated
              functions
Copyright   : (c) Mike Pilgrem 2019
License     : BSD 3-Clause
Maintainer  : public@pilgrem.com
Stability   : experimental

This module exports a type representing months in the Gregorian calendar
('YearMonth') and associated functions.
-}
module Data.Time.Calendar.YearMonth
  ( YearMonth (..)
  , toYearMonth
  , fromYearMonth
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, parseJSON, withText)
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Attoparsec.Text (Parser, char, digit)
import qualified Data.Attoparsec.Text as A
import Data.Bits((.&.))
import Data.Char (ord)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Format.ISO8601 (formatShow, yearMonthFormat)
import Servant.API (ToHttpApiData, toUrlPiece)


{- | Type representing months in the Gregorian calander.
-}

-- A type synonym is not used because of the need to have a specialised instance
-- of the 'Show' class.
newtype YearMonth = YearMonth (Integer, Int)
  deriving (Eq, Ord)

{- | @show yearMonth@ returns the month in the ISO 8601 @YYYY-MM@ format.
-}
instance Show YearMonth where
  show (YearMonth ym) = formatShow yearMonthFormat ym

{- | @toUrlPiece yearMonth@ returns the month in the ISO 8601 @YYYY-MM@ format.
-}
instance ToHttpApiData YearMonth where
  toUrlPiece = pack . show

{- | Assumes that the yearMonth is represented by a JSON string in the ISO 8601
@YYYY-MM@ format.
-}
instance FromJSON YearMonth where
  parseJSON = withText "YearMonth" (run yearMonth)

-- | Returns the specific month in which the given date falls.
toYearMonth :: Day -> YearMonth
toYearMonth date = YearMonth (y, m)
 where
  (y, m, _) = toGregorian date

-- | Returns a date for the specifc month, given the day of the month.
fromYearMonth
  :: Int        -- ^ The day of the month. The result will be clamped to the
                -- range from 1 to the last day of the month. Use
                -- @fromYearMonth 31@ for the last day of every month.
  -> YearMonth
  -> Day
fromYearMonth d (YearMonth (y, m)) = fromGregorian y m d
-- `fromGregorian` clips invalid values to the correct range, month first, then
-- day.

fromYearMonthValid :: (Integer, Int) -> Maybe YearMonth
fromYearMonthValid ym@(_, m)
  | m >= 1 && m <= 12 = Just (YearMonth ym)
  | otherwise =         Nothing

-- | Run an attoparsec parser as an aeson parser.
run :: Parser a -> Text -> Aeson.Parser a
run p t = case A.parseOnly (p <* A.endOfInput) t of
            Left err -> fail $ "could not parse date: " ++ err
            Right r  -> return r

-- | Parse a year and month of the form @YYYY-MM@.
yearMonth :: Parser YearMonth
yearMonth = do
  y <- (fourDigits <* char '-') <|> fail "year and month must be of form YYYY-MM"
  m <- twoDigits <|> fail "year and month must be of form YYYY-MM"
  maybe (fail "invalid date") pure (fromYearMonthValid (y, m))

-- | Parse a two-digit integer (e.g. month)
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  return $! c2d a * 10 + c2d b

-- | Parse a four-digit integer (e.g. year)
fourDigits :: Parser Integer
fourDigits = do
  a <- digit
  b <- digit
  c <- digit
  d <- digit
  return $! toInteger $ ((c2d a * 10 + c2d b) * 10 + c2d c) * 10 + c2d d

c2d :: Char -> Int
c2d c = ord c .&. 15
