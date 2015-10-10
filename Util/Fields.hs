module Util.Fields where

import Prelude
import Yesod.Core
import Yesod.Form.Types
import Yesod.Form.Fields
import Yesod.Form.Functions

usersFields :: (Eq a, RenderMessage site FormMessage) =>
              HandlerT site IO (OptionList a) -> Field (HandlerT site IO) [a]
usersFields ioptlist = (multiSelectField ioptlist)
  { fieldView =
       \theId name attrs val isReq -> do
           opts <- fmap olOptions $ handlerToWidget ioptlist
           let optselected (Left _) _ = False
               optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
           [whamlet|
            <span ##{theId}>
              $forall opt <- opts
                 <label>
                   <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                     #{optionDisplay opt}
            |]
  }
