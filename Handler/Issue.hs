module Handler.Issue where

import Prelude (read)

import Import as Import
import Yesod.Form.Bootstrap3
import Yesod.Goodies.PNotify

import Model.Fields

runForm = runFormPost . renderBootstrap3 Import.hGrid
genForm = generateFormPost . renderBootstrap3 Import.hGrid
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
      (w, enc) <- genForm $ issueForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue")
    -- simple
    simpleView render uid = do
      (w, enc) <- genForm $ simpleForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue-simple")
      
postNewIssueR :: Handler ()
postNewIssueR = do
  render <- getMessageRender
  uid <- requireAuthId
  ((r, _), _) <- runForm $ issueForm uid render Nothing
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

data Search = Search { query :: Maybe Text
                     , users :: [Entity User]
                     }

searchForm :: (AppMessage -> Text) -> Maybe Search -> AForm (HandlerT App IO) Search
searchForm render mv = Search
                       <$> aopt (searchField True) (bfs' $ render MsgUserNameOrIdent) (query <$> mv)
                       <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateGroup) "btn-primary" [])
                       <*> areq (checkboxesField collect) (bfs' $ render MsgUsers) (users <$> mv)
                       <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateGroup) "btn-primary" [])
  where
    collect :: Handler (OptionList (Entity User))
    collect = do
      entities <- runDB $ selectList [] [Asc UserIdent]
      optionsPersist [] [Asc UserIdent] userNameId

getNewChannelR :: IssueId -> Handler Html
getNewChannelR key = do
  render <- getMessageRender
  Just logic <- lookupGetParam "logic"
  Just mode <- lookupGetParam "mode"
  issue <- runDB $ get404 key
  (w, enc) <- genForm $ searchForm render Nothing
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "new-channel")

postNewChannelR :: IssueId -> Handler Html
postNewChannelR key = do
  creater <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  Just logic <- fmap (fmap fromText) $ lookupPostParam "logic"
  Just mode <- lookupPostParam "mode"
  ((r, _), _) <- runForm $ searchForm render Nothing
  case r of
    FormSuccess s -> do
      runDB $ do
        cid <- insert (Channel logic key)
        forM_ (users s) $ \u -> do
          let uid = entityKey u
          _ <- insert (Ticket cid creater uid uid now now)
          return ()
      redirect $ ISSUE $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
  where
    fromText :: Text -> Logic
    fromText = read . unpack
