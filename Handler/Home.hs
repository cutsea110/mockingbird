module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgHomeOf $ entityVal u
    $(widgetFile "homepage")
