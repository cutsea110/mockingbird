module Handler.Home where

import Import hiding (Status)
import Model.Fields
import Handler.Issue.Form (selfIssueForm)

getMyTasksR :: Handler Html
getMyTasksR = do
  uid <- requireAuthId
  getTasksR uid

getMyTimelineR :: Handler Html
getMyTimelineR = do
  uid <- requireAuthId
  getTimelineR uid

getComments :: MonadIO m =>
               UserId -> ReaderT SqlBackend m [(Entity Comment, Speaker, Maybe [Entity StoredFile], Status)]
getComments uid = do
  ts <- selectList ([TicketDomain ==. uid] ||. [TicketCodomain ==. uid] ||. [TicketAssign ==. uid]) []
  cs <- selectList [CommentTicket <-. map entityKey ts] [Desc CommentCreated, LimitTo 10]
  forM cs $ \comment@(Entity cid c) -> do
    u <- get404 $ commentSpeaker c
    mf <- if commentAttached c > 0
          then do
            fs <- selectList [StoredFileComment ==. cid] []
            return (Just fs)
          else return Nothing
    t <- get404 $ commentTicket c
    status <- channelStatus $ ticketChannel t
    return (comment, u, mf, status)

getTimelineR :: UserId -> Handler Html
getTimelineR uid = do
  Entity _ u <- requireAuth
  now <- liftIO getCurrentTime
  comments <- runDB $ getComments uid
  let createdBefore c = (commentCreated c) `beforeFrom` now
  defaultLayout $ do
    setTitleI $ MsgTimelineOf u
    $(widgetFile "timeline")

type AssignedTicket = (Entity Ticket, (Issue, Opener, Codomain), Maybe (Entity Comment, Speaker))

getAssignedActiveChannelIds :: MonadIO m => UserId -> ReaderT SqlBackend m [ChannelId]
getAssignedActiveChannelIds uid = do
  ticks <- selectList [TicketAssign ==. uid, TicketStatus ==. OPEN] []
  chans <- selectList [ChannelId <-. map (ticketChannel.entityVal) ticks] []
  cids <- forM chans $ \(Entity cid c) -> do
    case channelType c of
      ALL -> return [cid]
      ANY -> do
        closed <- count [TicketChannel ==. cid, TicketStatus ==. CLOSE]
        return $ if closed > 0 then [] else [cid]
  return $ concat cids

getAssignedTickets :: MonadIO m => UserId -> ReaderT SqlBackend m [AssignedTicket]
getAssignedTickets uid = do
  cids <- getAssignedActiveChannelIds uid
  ticks <- selectList [TicketAssign ==. uid, TicketStatus ==. OPEN, TicketChannel <-. cids] []
  forM ticks $ \tick@(Entity tid t) -> do
    let cid = ticketChannel t
    ch <- get404 cid
    let key = channelIssue ch
    issue <- get404 key
    op <- get404 $ issueOpener issue
    cod <- get404 $ ticketCodomain t
    mcom <- selectFirst [CommentTicket ==. tid] [Desc CommentCreated]
    msp <- maybe (return Nothing) (get . commentSpeaker . entityVal) mcom
    return (tick, (issue, op, cod), mcom >< msp)
  where
    (><) :: Maybe x -> Maybe y -> Maybe (x, y)
    Just x >< Just y = Just (x, y)
    _ >< _ = Nothing

getTasksR :: UserId -> Handler Html
getTasksR uid = do
  Entity myuid myself <- requireAuth
  render <- getMessageRender
  now <- liftIO getCurrentTime
  (u, ticks) <- runDB $ do
    u <- get404 uid
    ticks <- getAssignedTickets uid
    return (u, ticks)
  let tickets = sortBy (\x y -> sorter x y <> sorter2 x y) ticks
  ((_, w), enc) <- runFormInline $ selfIssueForm myuid render Nothing
  defaultLayout $ do
    setTitleI $ MsgTasksOf u
    $(widgetFile "tasks")
  where
    sorter = sorter' `on` issueLimitDatetime . acc
    sorter2 = compare `on` issueUpdated . acc
    sorter' (Just d1) (Just d2) = d1 `compare` d2
    sorter' Nothing _ = GT
    sorter' _ Nothing = LT
    acc = fst3 . snd3

putCloseTicketR :: TicketId -> Handler ()
putCloseTicketR tid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  runDB $ do
    tick <- get404 tid
    if uid `elem` [ticketAssign tick, ticketDomain tick, ticketCodomain tick]
      then do
        insert $ Comment tid (Textarea $ render MsgCloseTicket) uid 0 now now
        update tid [TicketStatus =. CLOSE, TicketUpdated =. now]
      else invalidArgs [render MsgYouCouldnotTouchTheTicket]
  redirect MyTasksR
