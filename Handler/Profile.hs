module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

getProfileR :: UserId -> Handler Html
getProfileR uid = do
  u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgProfileOf $ entityVal u
    $(widgetFile "homepage")
