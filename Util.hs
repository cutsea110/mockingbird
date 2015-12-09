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
       , whenJust_
       )where

import Prelude (read)
import ClassyPrelude.Yesod
import Data.Text as T
import Data.Time
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


filesField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
              Either (Route (HandlerSite m)) Text -> Either (Route (HandlerSite m)) Text -> Field m [FileInfo]
filesField jqueryJs faCss = Field
  { fieldParse = \_ files -> return $
      case files of
        [] -> Right Nothing
        fs@(_:_) -> Right $ Just fs
  , fieldView = \id' name attrs _ isReq -> do
     addScriptEither jqueryJs
     addStylesheetEither faCss
     toWidget [hamlet|
                 <div class="input-group">
                   <input name=#{name} *{attrs} type=file multiple style="display: none;" :isReq:required>
                   <span class="input-group-btn">
                     <button class="btn btn-default" type=button onclick="$(this).parent().prev().click();$(this).blur()">
                       <i class="fa fa-folder-open">
                   <div class="input-prepend">
                     <input type="text" class="form-control" *{attrs} disabled>
               |]
     toWidget [julius|
               $(function(){
                    $("input[name="+#{toJSON name}+"]")
                    .on("change", function(e) {
                       var fileInputs = $("input[name="+#{toJSON name}+"]").length,
                           fileSelects = $("input[name="+#{toJSON name}+"]")
                                         .map(function(){return this.files[0]}).length;
                       if (fileInputs <= fileSelects) {
                         var group = $(this).parent('div.input-group');
                         group.clone(true).insertAfter(group);
                       }
                       $(this).next().next().children('input[type=text]').val($(this).val());
                    });
               })
               |]
  , fieldEnctype = Multipart
  }

whenJust_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust_ mx f = maybe (return ()) f mx
