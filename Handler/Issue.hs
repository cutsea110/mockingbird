module Handler.Issue where

import Import as Import
import Yesod.Form.Bootstrap3
import Yesod.Goodies.PNotify

issueForm :: (MonadHandler m, RenderMessage (HandlerSite m) msg, RenderMessage (HandlerSite m) FormMessage) =>
     UserId -> (AppMessage -> msg) -> Maybe Issue -> AForm m Issue
issueForm uid render mv = Issue
                          <$> areq textField (bfs $ render MsgIssueSubject) (issueSubject <$> mv)
                          <*> aopt textareaField (bfs $ render MsgIssueDescription) (issueDescription <$> mv)
                          <*> aopt dayField (bfs $ render MsgIssueLimitDay) (issueLimitdate <$> mv)
                          <*> aopt timeFieldTypeTime (bfs $ render MsgIssueLimitTime) (issueLimittime <$> mv)
                          <*> pure uid
                          <*> lift (liftIO getCurrentTime)
                          <*> lift (liftIO getCurrentTime)
                          <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateIssue) "btn-primary" [])

getNewIssueR :: Handler Html
getNewIssueR = do
  render <- getMessageRender
  uid <- requireAuthId
  (w, enc) <- generateFormPost $ renderBootstrap3 Import.hGrid $ issueForm uid render Nothing
  defaultLayout $ do
    setTitleI MsgCreateNewIssue
    $(widgetFile "new-issue")

postNewIssueR :: Handler ()
postNewIssueR = do
  render <- getMessageRender
  uid <- requireAuthId
  ((r, _), _) <- runFormPost $ renderBootstrap3 Import.hGrid $ issueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      iid <- runDB $ insert issue
      setPNotify $ defNotify { _type = Just Success
                             , _title = Just $ Right $ render MsgSuccess
                             , _text = Just $ Right $ render MsgSucceedToCreateIssue
                             }
      redirect (ISSUE $ IssueR iid)
    FormFailure (x:_) -> do
      setPNotify $ defNotify { _type = Just Error
                             , _title = Just $ Right $ render MsgError
                             , _text = Just $ Right x
                             }
    _ -> do
      setPNotify $ defNotify { _type = Just Error
                             , _title = Just $ Right $ render MsgError
                             , _text = Just $ Right $ render MsgFailureToCreateIssue
                             }
  redirect (ISSUE NewIssueR)

getIssueR :: IssueId -> Handler Html
getIssueR key = do
  (issue, opener) <- runDB $ do
    issue <- get404 key
    opener <- get404 (issueOpener issue)
    return (issue, opener)
  now <- liftIO getCurrentTime
  let createdBefore = (issueCreated issue) `beforeFrom` now
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "issue")
