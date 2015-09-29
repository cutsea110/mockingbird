module Handler.Top where

import Import

getTopR :: Handler Html
getTopR = do
  mu <- maybeAuthId
  defaultLayout $ do
    setTitleI MsgTop
    $(widgetFile "top")
