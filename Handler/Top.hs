module Handler.Top where

import Import

getTopR :: Handler Html
getTopR = redirect MyTasksR
