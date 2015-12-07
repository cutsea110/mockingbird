module Util
       ( showText
       , readText
       , showDate
       , localDayToUTC
       , day'timeToUTC
       , Diff(..)
       , Limited(..)
       , beforeFrom
       , limitedBy
       , runForm
       , runFormInline
       , genForm
       , genFormInline
       , bfs'
       , bfs'focus
       , module Util.Fields
       , fst3
       , snd3
       , thd3

       , filesField
       , encodeUrl
       , decodeUrl

       , whenJust_
       )where

import Prelude (read)
import ClassyPrelude.Yesod hiding (urlEncode, urlDecode)

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.Text as T
import Data.Time
import Network.HTTP.Base (urlEncode, urlDecode)
import Yesod.Form.Bootstrap3

import Settings as Settings
import Util.Fields

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

data Limited = TimeOut Diff
             | InTime Diff
             deriving (Show, Read, Eq, Ord)

limitedBy :: UTCTime -> UTCTime -> Limited
now `limitedBy` limit = if now < limit
                        then InTime (now `beforeFrom` limit)
                        else TimeOut (limit `beforeFrom` now)

hGrid :: BootstrapFormLayout
hGrid = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)

runFormInline :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
                 AForm m a -> m ((FormResult a, WidgetT (HandlerSite m) IO ()), Enctype)
runFormInline = runFormPost . renderBootstrap3 BootstrapInlineForm

runForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
           AForm m a -> m ((FormResult a, WidgetT (HandlerSite m) IO ()), Enctype)
runForm = runFormPost . renderBootstrap3 hGrid

genFormInline :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
                 AForm m a -> m (WidgetT (HandlerSite m) IO (), Enctype)
genFormInline = generateFormPost . renderBootstrap3 BootstrapInlineForm

genForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
           AForm m a -> m (WidgetT (HandlerSite m) IO (), Enctype)
genForm = generateFormPost . renderBootstrap3 hGrid

bfs' :: Text -> Text -> FieldSettings site
bfs' lbl ph = withPlaceholder ph $ bfs lbl

bfs'focus :: Text -> Text -> FieldSettings site
bfs'focus lbl ph = withAutofocus $ bfs' lbl ph


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z


filesField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m [FileInfo]
filesField = Field
  { fieldParse = \_ files -> return $
      case files of
        [] -> Right Nothing
        fs@(_:_) -> Right $ Just fs
  , fieldView = \id' name attrs _ isReq -> do
     toWidget [hamlet|
                 <input name=#{name} *{attrs} type=file multiple :isReq:required>
               |]
     toWidget [julius|
               $(function(){
                    $("input[name="+#{toJSON name}+"]")
                    .on("change", function() {
                       var fileInputs = $("input[name="+#{toJSON name}+"]").length,
                           fileSelects = $("input[name="+#{toJSON name}+"]")
                                         .map(function(){return this.files[0]}).length;
                       if (fileInputs <= fileSelects) {
                         $(this).clone(true).insertAfter(this);
                       }
                    });
               })
               |]
  , fieldEnctype = Multipart
  }

encodeUrl :: String -> String
encodeUrl = urlEncode . encodeString

decodeUrl :: String -> String
decodeUrl = decodeString . urlDecode

whenJust_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust_ mx f = maybe (return ()) f mx
