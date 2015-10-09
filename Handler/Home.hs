module Handler.Home where

import Import

getTimelineR :: UserId -> Handler Html
getTimelineR _ = do
  Entity _ u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgTimelineOf u
    $(widgetFile "timeline")

getTaskR :: UserId -> Handler Html
getTaskR _ = do
  Entity _ u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgTasksOf u
    $(widgetFile "tasks")
