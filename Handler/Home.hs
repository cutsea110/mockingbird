module Handler.Home where

import Import

getTimelineR :: UserId -> Handler Html
getTimelineR uid = do
  Entity _ u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgTimelineOf u
    $(widgetFile "timeline")

getTaskR :: UserId -> Handler Html
getTaskR uid = do
  Entity _ u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgTasksOf u
    $(widgetFile "tasks")
