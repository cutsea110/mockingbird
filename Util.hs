module Util where

import Prelude (read)
import ClassyPrelude.Yesod

import Data.Text as T
import Data.Time

import Settings as Settings

showText :: (Show a) => a -> Text
showText = T.pack . show
readText :: (Read a) => Text -> a
readText = read . T.unpack

showDate :: UTCTime -> Text
showDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utc2local
    where
      utc2local = utcToLocalTime $ hoursToTimeZone Settings.tz

localDayToUTC :: Day -> UTCTime
localDayToUTC = localTimeToUTC (hoursToTimeZone Settings.tz) . flip LocalTime (TimeOfDay 0 0 0)

day'timeToUTC :: Day -> TimeOfDay -> UTCTime
day'timeToUTC = (localTimeToUTC (hoursToTimeZone Settings.tz) .) . LocalTime
