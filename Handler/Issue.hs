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


type Opener = User
type Codomain = User

getIssueTree :: MonadIO m => IssueId -> ReaderT SqlBackend m (Issue, Opener, [(Channel, [(Ticket, Codomain)])])
getIssueTree key = do
  issue <- get404 key
  opener <- get404 (issueOpener issue)
  cs <- selectList [ChannelIssue ==. key] []
  chans <- forM cs $ \(Entity cid c) -> do
    ts <- selectList [TicketChannel ==. cid] []
    tu <- forM ts $ \(Entity tid t) -> do
      u <- get404 (ticketCodomain t)
      return (t, u)
    return (c, tu)
  return (issue, opener, chans)


getIssueR :: IssueId -> Handler Html
getIssueR key = do
  (issue, opener, chans) <- runDB $ getIssueTree key
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
  Just logic <- fmap (fmap fromText) $ lookupPostParam "logic"
  Just mode <- lookupPostParam "mode"
  now <- liftIO getCurrentTime
  ((r, _), _) <- runForm $ searchForm render Nothing
  case r of
    FormSuccess s -> do
      runDB $ create logic mode (users s) now creater
      redirect $ ISSUE $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
  where
    fromText :: Text -> Logic
    fromText = read . unpack
    
    create :: MonadIO m =>
              Logic -> Text -> [Entity User] -> UTCTime -> Key User
              -> ReaderT SqlBackend m ()
    create logic mode us now creater =
      case mode of
        "one" -> do
          cid <- insert (Channel logic key)
          forM_ us $ \u -> do
            let uid = entityKey u
            _ <- insert (Ticket cid creater uid uid now now)
            return ()
        "each" -> do
          forM_ us $ \u -> do
            let uid = entityKey u
            cid <- insert (Channel logic key)
            _ <- insert (Ticket cid creater uid uid now now)
            return ()
        "self" -> do
          cid <- insert (Channel logic key)
          _ <- insert (Ticket cid creater creater creater now now)
          return ()

getChannelR :: IssueId -> ChannelId -> Handler Html
getChannelR = undefined

postChannelR :: IssueId -> ChannelId -> Handler Html
postChannelR = undefined
