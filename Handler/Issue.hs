module Handler.Issue ( getNewIssueR
                     , postNewIssueR
                     , getEditIssueR
                     , putEditIssueR
                     , postNewChannelR
                     , postCreateIssueR
                     , postAddSelfChannelR
                     , getIssueR
                     , deleteIssueR
                     , getAddChannelR
                     , postAddChannelR
                     , getChannelR
                     , putChannelR
                     , deleteChannelR
                     , getCloneIssueR
                     , postCloneIssueR
                     ) where

import Import as Import hiding ((\\))
import Data.List ((\\))

import Handler.Issue.Form
import Model.Fields
import Util.Widget (wGravatar', wCreatedBefore)

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
    "QUICK" -> createQuickIssue >> redirect MyTasksR
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
      runDB $ replace key issue { issueUpdated = now }
      redirect $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

postNewChannelR :: Handler Html
postNewChannelR = do
  uid <- requireAuthId
  render <- getMessageRender
  ((r, _), _) <- runForm $ issueForm uid render Nothing
  (allCheckBtnId, allUncheckBtnId) <- (,) <$> newIdent <*> newIdent
  case r of
    FormSuccess issue -> do
      (w, enc) <- genFormInline $ searchAndHiddenIssueForm uid render (Just issue) Nothing
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

createQuickIssue :: Handler IssueId
createQuickIssue = do
  uid <- requireAuthId
  render <- getMessageRender
  ((r, _), _) <- runForm $ selfIssueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      let mdesc = maybe (Just $ Textarea $ issueSubject issue) Just (issueDescription issue)
      key <- runDB $ insert issue { issueDescription = mdesc, issueScope = PUBLIC }
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
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "issue")

deleteIssueR :: IssueId -> Handler ()
deleteIssueR key = do
  uid <- requireAuthId
  runDB $ do
    cs <- selectList [ChannelIssue ==. key] []
    deleteWhere [TicketChannel <-. map entityKey cs]
    deleteWhere [ChannelIssue ==. key]
    delete key
  redirect MyTasksR

getAddChannelR :: IssueId -> Handler Html
getAddChannelR key = do
  render <- getMessageRender
  issue <- runDB $ get404 key
  (w, enc) <- genFormInline $ searchForm render Nothing
  (allCheckBtnId, allUncheckBtnId) <- (,) <$> newIdent <*> newIdent
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
  (allCheckBtnId, allUncheckBtnId) <- (,) <$> newIdent <*> newIdent
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
        tids <- insertMany $ map (\nid -> Ticket cid uid nid nid OPEN now now) $ news \\ olds
        c <- count [TicketChannel ==. cid]
        when (c == 0) $ delete cid
      redirect $ IssueR key
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

deleteChannelR :: IssueId -> ChannelId -> Handler ()
deleteChannelR key cid = do
  runDB $ do
    ts <- selectList [TicketChannel ==. cid] []
    deleteWhere [TicketChannel ==. cid]
    delete cid
  redirect $ IssueR key

getCloneIssueR :: IssueId -> Handler Html
getCloneIssueR key = do
  uid <- requireAuthId
  render <- getMessageRender
  (issue, opener, chans) <- runDB $ getIssueTree key
  now <- liftIO getCurrentTime
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

create :: MonadIO m =>
          IssueId -> Logic -> Mode -> Maybe [Entity User] -> UTCTime -> UserId
          -> ReaderT SqlBackend m ()
create key logic mode mus now creater = do
  when (maybe True null mus) $ do
    return ()
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
  
