module Handler.Issue where

import Prelude (read)

import Import as Import
import Yesod.Form.Bootstrap3
import Yesod.Goodies.PNotify

import Model.Fields


bfs' = withPlaceholder <*> bfs
bfs'focus = withAutofocus <$> bfs'

issueForm
  :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
     Key User -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
issueForm uid render mv = Issue
                          <$> areq textField (bfs'focus $ render MsgIssueSubject) (issueSubject <$> mv)
                          <*> aopt textareaField (bfs' $ render MsgIssueDescription) (issueDescription <$> mv)
                          <*> aopt dayField (bfs' $ render MsgIssueLimitDay) (issueLimitdate <$> mv)
                          <*> aopt timeFieldTypeTime (bfs' $ render MsgIssueLimitTime) (issueLimittime <$> mv)
                          <*> pure uid
                          <*> lift (liftIO getCurrentTime)
                          <*> lift (liftIO getCurrentTime)
                          <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateIssue) "btn-primary" [])

simpleForm
  :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
     Key User -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
simpleForm uid render mv = Issue
                          <$> areq textField (bfs'focus $ render MsgIssueSubject) (issueSubject <$> mv)
                          <*> pure Nothing
                          <*> pure Nothing
                          <*> pure Nothing
                          <*> pure uid
                          <*> lift (liftIO getCurrentTime)
                          <*> lift (liftIO getCurrentTime)
                          <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateIssue) "btn-primary" [])

getNewIssueR :: Handler Html
getNewIssueR = do
  render <- getMessageRender
  uid <- requireAuthId
  mode <- lookupGetParam "mode"
  case mode of
    Just "simple" -> simpleView render uid
    Just _ -> defaultView render uid
    Nothing -> defaultView render uid
  where
    -- default
    defaultView render uid = do
      (w, enc) <- generateFormPost $ renderBootstrap3 Import.hGrid $ issueForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue")
    -- simple
    simpleView render uid = do
      (w, enc) <- generateFormPost $ renderBootstrap3 BootstrapInlineForm $ simpleForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue-simple")
      
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

data Search = Seach { query :: Text } deriving Show

searchForm render mv = Seach
                       <$> areq (searchField True) (bfs' $ render MsgUserNameOrIdent) (query <$> mv)
                       <*  bootstrapSubmit (BootstrapSubmit (render MsgSearch) "btn-primary" [])

getNewChannelR :: IssueId -> Handler Html
getNewChannelR key = do
  render <- getMessageRender
  Just logic <- lookupGetParam "logic"
  let uri = (ISSUE $ NewChannelR key, [("logic", logic)])
  issue <- runDB $ get404 key
  defaultLayout $ do
    (w, enc) <- generateFormPost $ renderBootstrap3 Import.hGrid $ searchForm render Nothing
    setTitleI $ MsgSubject issue
    $(widgetFile "new-channel")

postNewChannelR :: IssueId -> Handler Html
postNewChannelR key = undefined
