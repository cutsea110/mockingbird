module Handler.Top where

import Import

getTopR :: Handler Html
getTopR = do
  defaultLayout $ do
    setTitleI MsgTop
    $(widgetFile "top")
