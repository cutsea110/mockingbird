module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
