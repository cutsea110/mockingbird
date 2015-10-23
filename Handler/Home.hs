module Handler.Home where

import Import
import Model.Fields

getMyTasksR :: Handler Html
getMyTasksR = do
  uid <- requireAuthId
  getTasksR uid

getMyTimelineR :: Handler Html
getMyTimelineR = do
  uid <- requireAuthId
  getTimelineR uid

getComments :: MonadIO m => Key User -> ReaderT SqlBackend m [Entity Comment]
getComments uid = do
  ts <- selectList ([TicketDomain ==. uid] ||. [TicketCodomain ==. uid] ||. [TicketAssign ==. uid]) []
  selectList [CommentTicket <-. map entityKey ts] [Desc CommentCreated]

getTimelineR :: UserId -> Handler Html
getTimelineR uid = do
  Entity _ u <- requireAuth
  comments <- runDB $ getComments uid
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
  Entity _ u <- requireAuth
  now <- liftIO getCurrentTime
  ticks <- runDB $ getAssignedTickets uid
  let tickets = sortBy (\x y -> sorter x y <> sorter2 x y) ticks
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
