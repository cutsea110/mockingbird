module Util where

import Prelude (read)
import ClassyPrelude.Yesod

import Data.Text as T
import Data.Time
import Yesod.Form.Bootstrap3

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

data Diff = Seconds Integer
          | Minutes Integer
          | Hours Integer
          | Days Integer
          | Months Integer
          | Years Integer
          deriving (Show, Read, Eq, Ord)

beforeFrom :: UTCTime -> UTCTime -> Diff
t `beforeFrom` now =
  let (s, d) = (round $ utctDayTime now - utctDayTime t, utctDay now `diffDays` utctDay t)
  in if d == 0
     then if s < 60
          then Seconds s
          else if s < 60 * 60
               then Minutes $ s `div` 60
               else Hours $ s `div` (60 * 60)
     else if d < 30
          then Days $ fromIntegral d
          else if d < 365
               then Months $ fromIntegral d `div` 30
               else Years $ fromIntegral d `div` 365

hGrid :: BootstrapFormLayout
hGrid = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)

runForm = runFormPost . renderBootstrap3 hGrid
genForm = generateFormPost . renderBootstrap3 hGrid
bfs' = withPlaceholder <*> bfs
bfs'focus = withAutofocus <$> bfs'
