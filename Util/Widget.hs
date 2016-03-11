module Util.Widget ( wGravatar
                   , wGravatar'
                   , wGravatarTiny'

                   , wGravatarRoute
                   , wGravatarRoute'
                   , wGravatarRouteTiny'

                   , limitDiffBadge
                   )where

import Import
import Yesod.Core

wGravatar :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => User -> WidgetT site m ()
wGravatar = wGravatarRoute (Right ("https://gravatar.com"::Text))

wGravatar' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => User -> WidgetT site m ()
wGravatar' = wGravatarRoute' (Right ("https://gravatar.com"::Text)) True

wGravatarTiny' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => User -> WidgetT site m ()
wGravatarTiny' = wGravatarRouteTiny' (Right ("https://gravatar.com"::Text))

wGravatarRoute :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
                  Either (Route site) Text -> User -> WidgetT site m ()
wGravatarRoute r = wGravatarRoute' r False

wGravatarRoute' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
                  Either (Route site) Text -> Bool -> User -> WidgetT site m ()
wGravatarRoute' (Right r) b u =
  [whamlet|<a href="#{r}" class="media-left">
             <img class="hidden-xs" src="#{userGravatar u}">
             <img class="visible-xs" src="#{userGravatarSmall u}">
             $if b
               <span class="small text-muted">#{userName u}
          |]
wGravatarRoute' (Left r) b u =
  [whamlet|<a href="@{r}" class="media-left">
             <img class="hidden-xs" src="#{userGravatar u}">
             <img class="visible-xs" src="#{userGravatarSmall u}">
             <span class="small text-muted">#{userName u}
             $if b
               <span class="small text-muted">#{userName u}
          |]

wGravatarRouteTiny' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
                       Either (Route site) Text -> User -> WidgetT site m ()
wGravatarRouteTiny' (Right r) u =
  [whamlet|<a href="#{r}">
             <img class="gravatar" src="#{userGravatarTiny u}" alt=Gravatar>
             <span class="small text-muted">#{userName u}
          |]
wGravatarRouteTiny' (Left r) u =
  [whamlet|<a href="@{r}">
             <img class="gravatar" src="#{userGravatarTiny u}" alt=Gravatar>
             <span class="small text-muted">#{userName u}
          |]

data TimeUp = I | O deriving (Show, Read, Eq, Ord)

limitDiffBadge :: (MonadIO m, RenderMessage site AppMessage, MonadBaseControl IO m, MonadThrow m) =>
                  UTCTime -> UTCTime -> WidgetT site m ()
limitDiffBadge now limit =
  case limitDiff of
    (I, msg, smsg) ->
        [whamlet|
         <span class="label label-warning pull-right hidden-xs">_{msg}
         <span class="label label-warning pull-right visible-xs">_{smsg}
         |]
    (O, msg, smsg) ->
        [whamlet|
         <span class="label label-danger pull-right hidden-xs">_{msg}
         <span class="label label-danger pull-right visible-xs">_{smsg}
         |]
    where
      limitDiff =
          case limitedBy now limit of
            InTime diff ->
              case diff of
                Seconds n -> (I, MsgSecondsLeft n, MsgSecondsLeftShort n)
                Minutes n -> (I, MsgMinutesLeft n, MsgMinutesLeftShort n)
                Hours n -> (I, MsgHoursLeft n, MsgHoursLeftShort n)
                Days n -> (I, MsgDaysLeft n, MsgDaysLeftShort n)
                Months n -> (I, MsgMonthsLeft n, MsgMonthsLeftShort n)
                Years n -> (I, MsgYearsLeft n, MsgYearsLeftShort n)
            TimeOut diff ->
              case diff of
                Seconds n -> (O, MsgSecondsOver n, MsgSecondsOverShort n)
                Minutes n -> (O, MsgMinutesOver n, MsgMinutesOverShort n)
                Hours n -> (O, MsgHoursOver n, MsgHoursOverShort n)
                Days n -> (O, MsgDaysOver n, MsgDaysOverShort n)
                Months n -> (O, MsgMonthsOver n, MsgMonthsOverShort n)
                Years n -> (O, MsgYearsOver n, MsgYearsOverShort n)
