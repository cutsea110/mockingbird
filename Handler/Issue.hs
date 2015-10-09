module Handler.Issue where

import Prelude (read)

import Import as Import hiding ((\\))
import Yesod.Form.Bootstrap3
import Yesod.Goodies.PNotify
import Data.List ((\\))
import Data.Time.LocalTime

import Model.Fields

runFormInline = runFormPost . renderBootstrap3 BootstrapInlineForm
runForm = runFormPost . renderBootstrap3 Import.hGrid
genForm = generateFormPost . renderBootstrap3 Import.hGrid
bfs' = withPlaceholder <*> bfs
bfs'focus = withAutofocus <$> bfs'

issueForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
             UserId -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
issueForm uid render mv = Issue
                          <$> areq textField (bfs'focus $ render MsgIssueSubject) (issueSubject <$> mv)
                          <*> aopt textareaField (bfs' $ render MsgIssueDescription) (issueDescription <$> mv)
                          <*> aopt dayField (bfs' $ render MsgIssueLimitDay) (issueLimitdate <$> mv)
                          <*> aopt timeFieldTypeTime (bfs' $ render MsgIssueLimitTime) (issueLimittime <$> mv)
                          <*> pure uid
                          <*> lift (liftIO getCurrentTime)
                          <*> lift (liftIO getCurrentTime)
                          <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateIssue) "btn-primary" [])

hiddenIssueForm uid mv = Issue
                         <$> areq hiddenField "" (issueSubject <$> mv)
                         <*> aopt hiddenField "" (issueDescription <$> mv)
                         <*> aopt hiddenField "" (issueLimitdate <$> mv)
                         <*> aopt hiddenField "" (issueLimittime <$> mv)
                         <*> pure uid
                         <*> lift (liftIO getCurrentTime)
                         <*> lift (liftIO getCurrentTime)

selfForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
            UserId -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
selfForm uid render mv = Issue
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
  uid <- requireAuthId
  render <- getMessageRender
  mode <- lookupGetParam "mode"
  case mode of
    Just "self" -> selfView render uid
    Just _ -> defaultView render uid
    Nothing -> defaultView render uid
  where
    -- default
    defaultView render uid = do
      (w, enc) <- genForm $ issueForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue")
    -- self
    selfView render uid = do
      (w, enc) <- genForm $ selfForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue-self")

postNewIssueR :: Handler Html
postNewIssueR = do
  uid <- requireAuthId
  render <- getMessageRender
  ((r, _), _) <- runForm $ issueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      ((_, w), enc) <- runFormInline $ hiddenIssueForm uid (Just issue)
      opener <- runDB $ get404 uid
      let chans = []
      defaultLayout $ do
        setTitleI MsgCreateIssue
        $(widgetFile "issue-on-the-fly")
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

postNewIssueChanR :: Handler Html
postNewIssueChanR = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  Just logic <- fmap (fmap fromText) $ lookupPostParam "logic"
  Just mode <- lookupPostParam "mode"
  ((r, _), _) <- runForm $ searchAndHiddenIssueForm uid render Nothing Nothing
  case r of
    FormSuccess (issue, s) -> do
      iid <- runDB $ do
        iid <- insert issue
        create iid logic mode (users s) now uid
        return iid
      redirect $ ISSUE $ IssueR iid
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
  where
    fromText :: Text -> Logic
    fromText = read . unpack

postNewSelfIssueR :: Handler ()
postNewSelfIssueR = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  ((r, _), _) <- runForm $ selfForm uid render Nothing
  case r of
    FormSuccess issue -> do
      key <- runDB $ insert issue { issueDescription = Just $ Textarea $ issueSubject issue }
      postNewSelfChanR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

getCloneIssueR :: IssueId -> Handler Html
getCloneIssueR key = do
  uid <- requireAuthId
  render <- getMessageRender
  (issue, opener, chans) <- runDB $ getIssueTree key
  now <- liftIO getCurrentTime
  let createdBefore = (issueCreated issue) `beforeFrom` now
  (w, enc) <- genForm $ issueForm uid render Nothing
  defaultLayout $ do
    setTitleI MsgCloneIssue
    $(widgetFile "clone-issue")

postCloneIssueR :: IssueId -> Handler ()
postCloneIssueR key = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  (_, _, chans) <- runDB $ getIssueTree key
  ((r, _), _) <- runForm $ issueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      iid <- runDB $ do
        iid <- insert issue
        forM_ chans $ \(_, c, ts) -> do
          cid <- insert $ Channel (channelType c) iid
          let uids' = map (ticketCodomain . snd3) ts
          insertMany_ $ map (\uid' -> Ticket cid uid uid' uid' OPEN now now) uids'
        return iid
      redirect $ ISSUE $ IssueR iid
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
  where
    snd3 (x, y, z) = y

type Opener = User
type Codomain = User
type IssueTree = (Issue, Opener, [ChannelTree])
type ChannelTree = (ChannelId, Channel, [(TicketId, Ticket, Codomain)])

progress :: (Channel, [(TicketId, Ticket, Codomain)]) -> Int
progress (ch, ts) = case channelType ch of
                      ALL -> round $ 100 * num / den
                      ANY -> if any pred ts then 100 else 0
  where
   (den, num) = foldr (\(_, t, _) (ttl, cls) -> (ttl+1, if close t then cls+1 else cls)) (0, 0) ts
   pred (_, t, _) = close t

create :: MonadIO m =>
          IssueId -> Logic -> Text -> Maybe [Entity User] -> UTCTime -> UserId
          -> ReaderT SqlBackend m ()
create key logic mode mus now creater = do
  case mode of
    "one" -> do
      cid <- insert $ Channel logic key
      forM_ us $ \(Entity uid _) -> do
        insert_ $ Ticket cid creater uid uid OPEN now now
    "each" -> do
      forM_ us $ \(Entity uid _) -> do
        cid <- insert $ Channel logic key
        insert_ $ Ticket cid creater uid uid OPEN now now
    "self" -> do
      cid <- insert $ Channel logic key
      insert_ $ Ticket cid creater creater creater OPEN now now
  where
    us = maybe [] id mus

getIssueTree :: MonadIO m => IssueId -> ReaderT SqlBackend m IssueTree
getIssueTree key = do
  issue <- get404 key
  opener <- get404 (issueOpener issue)
  cs <- selectList [ChannelIssue ==. key] []
  chans <- forM cs $ \(Entity cid c) -> do
    ts <- selectList [TicketChannel ==. cid] []
    tu <- forM ts $ \(Entity tid t) -> do
      u <- get404 (ticketCodomain t)
      return (tid, t, u)
    return (cid, c, tu)
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
                     , users :: Maybe [Entity User]
                     }

searchAndHiddenIssueForm uid render mi ms
  = (,)
    <$> hiddenIssueForm uid mi
    <*> searchForm render ms

searchForm :: (AppMessage -> Text) -> Maybe Search -> AForm (HandlerT App IO) Search
searchForm render mv = Search
                       <$> aopt (searchField True) (bfs' $ render MsgUserNameOrIdent) (query <$> mv)
                       <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateGroup) "btn-primary" [])
                       <*> aopt (checkboxesField collect) (bfs' $ render MsgUsers) (users <$> mv)
  where
    collect :: Handler (OptionList (Entity User))
    collect = do
      entities <- runDB $ selectList [] [Asc UserIdent]
      optionsPersist [] [Asc UserIdent] userNameId

postNewChanR :: Handler Html
postNewChanR = do
  uid <- requireAuthId
  render <- getMessageRender
  Just logic <- lookupPostParam "logic"
  Just mode <- lookupPostParam "mode"
  ((r, _), _) <- runForm $ hiddenIssueForm uid Nothing
  case r of
    FormSuccess issue -> do
      ((_, w), enc) <- runForm $ searchAndHiddenIssueForm uid render (Just issue) Nothing
      defaultLayout $ do
        setTitleI MsgCreateIssue
        $(widgetFile "new-channel-on-the-fly")
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

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
      runDB $ create key logic mode (users s) now creater
      redirect $ ISSUE $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
  where
    fromText :: Text -> Logic
    fromText = read . unpack
    
postNewSelfChanR :: IssueId -> Handler ()
postNewSelfChanR key = do
  uid <- requireAuthId
  now <- liftIO getCurrentTime
  runDB $ do
    cid <- insert $ Channel ALL key
    insert_ $ Ticket cid uid uid uid OPEN now now
  redirect $ ISSUE $ IssueR key

getChannelR :: IssueId -> ChannelId -> Handler Html
getChannelR key cid = do
  render <- getMessageRender
  (issue, us) <- runDB $ do
    issue <- get404 key
    ts <- selectList [TicketChannel ==. cid] []
    us <- selectList [UserId <-. map (ticketCodomain.entityVal) ts] []
    return (issue, us)
  ((_, w), enc) <- runForm $ searchForm render $ Just (Search Nothing $ Just us)
  defaultLayout $ do
    setTitleI MsgUpdateChannel
    $(widgetFile "channel")

postChannelR :: IssueId -> ChannelId -> Handler Html
postChannelR key cid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  ((r, _), _) <- runForm $ searchForm render Nothing
  case r of
    FormSuccess s -> do
      let news = map entityKey $ maybe [] id $ users s
      runDB $ do
        ts <- selectList [TicketChannel ==. cid] []
        let olds = map (ticketCodomain.entityVal) ts
        deleteWhere [TicketChannel ==. cid, TicketCodomain /<-. news]
        insertMany_ $ map (\nid -> Ticket cid uid nid nid OPEN now now) $ news \\ olds
      redirect $ ISSUE $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

