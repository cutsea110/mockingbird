module Handler.Issue where

import Import as Import hiding ((\\))
import Data.List ((\\))

import Handler.Issue.Form
import Model.Fields

data Mode = ONE
          | SELF
          | EACH
          deriving (Show, Read, Eq, Ord)

getNewIssueR :: Handler Html
getNewIssueR = do
  uid <- requireAuthId
  render <- getMessageRender
  (w, enc) <- genForm $ issueForm uid render Nothing
  defaultLayout $ do
    setTitleI MsgCreateNewIssue
    $(widgetFile "new-issue")

postNewIssueR :: Handler Html
postNewIssueR = do
  Just mode <- lookupPostParam "mode"
  case mode of
    "SELF" -> createSelfIssue >>= \key -> redirect $ IssueR key
    "QUICK" -> createSelfIssue >> redirect MyTasksR
    _ -> postNewChannelR

getEditIssueR :: IssueId -> Handler Html
getEditIssueR key = do
  uid <- requireAuthId
  render <- getMessageRender
  issue <- runDB $ get404 key
  (w, enc) <- genForm $ issueForm uid render (Just issue)
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "edit-issue")

putEditIssueR :: IssueId -> Handler ()
putEditIssueR key = do
  uid <- requireAuthId
  render <- getMessageRender
  ((r, _), _) <- runForm $ issueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      now <- liftIO getCurrentTime
      runDB $ update key [ IssueSubject =. issueSubject issue
                         , IssueDescription =. issueDescription issue
                         , IssueLimitdate =. issueLimitdate issue
                         , IssueLimittime =. issueLimittime issue
                         , IssueUpdated =. now
                         ]
      redirect $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

postNewChannelR :: Handler Html
postNewChannelR = do
  uid <- requireAuthId
  render <- getMessageRender
  ((r, _), _) <- runForm $ issueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      ((_, w), enc) <- runFormInline $ searchAndHiddenIssueForm uid render (Just issue) Nothing
      defaultLayout $ do
        setTitleI MsgCreateIssue
        $(widgetFile "new-channel")
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

postCreateIssueR :: Handler Html
postCreateIssueR = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  Just md <- lookupPostParam "mode"
  let (logic, mode) = dispatch md
  ((r, _), _) <- runFormInline $ searchAndHiddenIssueForm uid render Nothing Nothing
  case r of
    FormSuccess (issue, s) -> do
      iid <- runDB $ do
        iid <- insert issue
        create iid logic mode (users s) now uid
        return iid
      redirect $ IssueR iid
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
  where
    dispatch "ALL" = (ALL, ONE)
    dispatch "ANY" = (ANY, ONE)
    dispatch "EACH" = (ALL, EACH)
    dispatch _ = (ALL, ONE)

createSelfIssue :: Handler IssueId
createSelfIssue = do
  uid <- requireAuthId
  render <- getMessageRender
  ((r, _), _) <- runForm $ issueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      let mdesc = maybe (Just $ Textarea $ issueSubject issue) Just (issueDescription issue)
      key <- runDB $ insert issue { issueDescription = mdesc }
      postAddSelfChannelR key
      return key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

postAddSelfChannelR :: IssueId -> Handler ()
postAddSelfChannelR key = do
  uid <- requireAuthId
  now <- liftIO getCurrentTime
  runDB $ do
    cid <- insert $ Channel ALL key
    insert_ $ Ticket cid uid uid uid OPEN now now
--  redirect $ IssueR key

getIssueR :: IssueId -> Handler Html
getIssueR key = do
  uid <- requireAuthId
  (issue, opener, chans) <- runDB $ getIssueTree key
  now <- liftIO getCurrentTime
  let createdBefore = (issueCreated issue) `beforeFrom` now
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "issue")

getAddChannelR :: IssueId -> Handler Html
getAddChannelR key = do
  render <- getMessageRender
  issue <- runDB $ get404 key
  (w, enc) <- genFormInline $ searchForm render Nothing
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "add-channel")

postAddChannelR :: IssueId -> Handler Html
postAddChannelR key = do
  creater <- requireAuthId
  render <- getMessageRender
  Just md <- lookupPostParam "mode"
  let (logic, mode) = dispatch md
  now <- liftIO getCurrentTime
  ((r, _), _) <- runFormInline $ searchForm render Nothing
  case r of
    FormSuccess s -> do
      runDB $ create key logic mode (users s) now creater
      redirect $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
  where
    dispatch "ALL" = (ALL, ONE)
    dispatch "ANY" = (ANY, ONE)
    dispatch "EACH" = (ALL, EACH)
    dispatch _ = (ALL, ONE)

getChannelR :: IssueId -> ChannelId -> Handler Html
getChannelR key cid = do
  render <- getMessageRender
  (chan, us) <- runDB $ do
    ch <- get404 cid
    ts <- selectList [TicketChannel ==. cid] []
    us <- selectList [UserId <-. map (ticketCodomain.entityVal) ts] []
    return (ch, us)
  ((_, w), enc) <- runFormInline $ searchForm render $ Just (Search $ Just us)
  defaultLayout $ do
    setTitleI MsgUpdateChannel
    $(widgetFile "channel")

putChannelR :: IssueId -> ChannelId -> Handler Html
putChannelR key cid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  ((r, _), _) <- runFormInline $ searchForm render Nothing
  case r of
    FormSuccess s -> do
      let news = map entityKey $ maybe [] id $ users s
      runDB $ do
        ts <- selectList [TicketChannel ==. cid] []
        let olds = map (ticketCodomain.entityVal) ts
        deleteWhere [TicketChannel ==. cid, TicketCodomain /<-. news]
        insertMany_ $ map (\nid -> Ticket cid uid nid nid OPEN now now) $ news \\ olds
      redirect $ IssueR key
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
      redirect $ IssueR iid
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

progress :: (Channel, [(TicketId, Ticket, Codomain)]) -> Int
progress (ch, ts) = case channelType ch of
                      ALL -> 100 * num `div` den
                      ANY -> if any closep ts then 100 else 0
  where
   (den, num) = foldr (\(_, t, _) (ttl, cls) -> (ttl+1, if close t then cls+1 else cls)) (0, 0) ts
   closep (_, t, _) = close t

create :: MonadIO m =>
          IssueId -> Logic -> Mode -> Maybe [Entity User] -> UTCTime -> UserId
          -> ReaderT SqlBackend m ()
create key logic mode mus now creater = do
  case mode of
    ONE -> do
      cid <- insert $ Channel logic key
      forM_ us $ \(Entity uid _) -> do
        insert_ $ Ticket cid creater uid uid OPEN now now
    EACH -> do
      forM_ us $ \(Entity uid _) -> do
        cid <- insert $ Channel logic key
        insert_ $ Ticket cid creater uid uid OPEN now now
    SELF -> do
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
  
