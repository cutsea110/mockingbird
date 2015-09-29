module Handler.Issue where

import Import as Import
import Data.Time (Day, TimeOfDay(..))
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3

issueForm
  :: (MonadHandler m, RenderMessage (HandlerSite m) msg, RenderMessage (HandlerSite m) FormMessage) =>
     (AppMessage -> msg) -> Maybe Issue -> AForm m Issue
issueForm render mv = Issue
                     <$> areq textField (bfs $ render MsgIssueSubject) (issueSubject <$> mv)
                     <*> aopt textareaField (bfs $ render MsgIssueDescription) (issueDescription <$> mv)
                     <*> aopt dayField (bfs $ render MsgIssueLimitDate) (issueLimitdate <$> mv)
                     <*> areq hiddenField (bfs $ render MsgIssueOpener) (issueOpener <$> mv)
                     <*> areq dayField (bfs $ render MsgIssueCreatedDay) (issueCreatedDay <$> mv)
                     <*> areq timeFieldTypeTime (bfs $ render MsgIssueCreatedTime) (issueCreatedTime <$> mv)
                     <*> areq dayField (bfs $ render MsgIssueUpdatedDay) (issueUpdatedDay <$> mv)
                     <*> areq timeFieldTypeTime (bfs $ render MsgIssueUpdatedTime) (issueUpdatedTime <$> mv)
                     <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateIssue) "btn-primary" [])

getNewIssueR :: Handler Html
getNewIssueR = do
  render <- getMessageRender
  (w, enc) <- generateFormPost $ renderBootstrap3 Import.hGrid $ issueForm render Nothing
  defaultLayout $ do
    setTitleI MsgCreateNewIssue
    $(widgetFile "new-issue")

postNewIssueR :: Handler ()
postNewIssueR = undefined
