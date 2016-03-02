module Handler.Top ( getTopR
                   ) where

import Import

getTopR :: Handler Html
getTopR = redirect MyTasksR
