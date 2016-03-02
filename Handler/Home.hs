module Handler.Home ( getMyTasksR
                    , getMyTimelineR
                    , getTimelineR
                    , getTimelineBeforeR
                    , getTasksR
                    , putCloseTicketR
                    , putReopenTicketR
                    ) where

import Import hiding (Status, sortBy)
import Data.List (sortBy)
import Data.Text as T (append)
import Database.Persist.Sql

import Model.Fields
import Handler.Common (toFullEquipedComments, toFullEquipedIssue)
import Handler.Issue.Form (selfIssueForm)

getMyTasksR :: Handler Html
getMyTasksR = do
  uid <- requireAuthId
  getTasksR uid

getMyTimelineR :: Handler Html
getMyTimelineR = do
  uid <- requireAuthId
  getTimelineR uid

commentsPerPage :: Int
commentsPerPage = 50

comments :: MonadIO m => UserId -> Maybe CommentId -> ReaderT SqlBackend m [Entity Comment]
comments uid mcid = rawSql sql param
    where
      param = [toPersistValue uid, toPersistValue uid] ++
              maybe [] (\cid -> [toPersistValue cid]) mcid ++
              [toPersistValue commentsPerPage]
      sql :: Sql
      sql = "WITH ts AS \
             (SELECT DISTINCT id \
              FROM ticket \
              WHERE channel IN (SELECT DISTINCT channel.id \
                                FROM channel, ticket \
                                WHERE \
                                      channel.id=ticket.channel \
                                  AND (ticket.domain=? OR ticket.codomain=?))) \
            SELECT \
               DISTINCT ?? \
            FROM \
               comment \
            WHERE \
                  comment.ticket IN (SELECT id FROM ts)"
             `T.append` maybe "" (const " AND comment.id < ?") mcid
             `T.append` " ORDER BY comment.created DESC LIMIT ?"

getComments :: MonadIO m =>
               UserId -> Maybe CommentId -> ReaderT SqlBackend m [FullEquipedComment]
getComments uid mcid = comments uid mcid >>= toFullEquipedComments

getTimelineR :: UserId -> Handler Html
getTimelineR uid = do
  Entity _ u <- requireAuth
  now <- liftIO getCurrentTime
  comments <- runDB $ getComments uid Nothing
  let createdBefore c = (commentCreated c) `beforeFrom` now
  defaultLayout $ do
    setTitleI $ MsgTimelineOf u
    $(widgetFile "timeline")

getTimelineBeforeR :: UserId -> CommentId -> Handler Value
getTimelineBeforeR uid cid = do
  (Entity _ u, uR, mR) <- (,,) <$> requireAuth <*> getUrlRender <*> getMessageRender
  now <- liftIO getCurrentTime
  comments <- runDB $ getComments uid (Just cid)
  let createdBefore c = (commentCreated c) `beforeFrom` now
  returnJson $ object [ "comments" .= array (map (go createdBefore uR mR) comments) ]
  where
    go cb ur mr (issue, Entity cid' com, spkr, opp, msf, st)
      = object [ "userGravatar" .= userGravatar spkr
               , "userName" .= userName spkr
               , "createdBefore" .= case cb com of
                                      Seconds n -> mr (MsgSecondsAgo n)
                                      Minutes n -> mr (MsgMinutesAgo n)
                                      Hours n -> mr (MsgHoursAgo n)
                                      Days n -> mr (MsgDaysAgo n)
                                      Months n -> mr (MsgMonthsAgo n)
                                      Years n -> mr (MsgYearsAgo n)
               , "threadUrl" .= ur (ThreadR $ commentTicket com)
               , "status" .= show st
               , "subject" .= issueSubject issue
               , "comment" .= toJSON (commentComment com)
               , "opponentGravatar" .= userGravatarTiny opp
               , "opponentName" .= userName opp
               , "nextUrl" .= ur (TimelineBeforeR uid cid')
               , "storedFiles" .= array (maybe [] (map (go' ur)) msf)
               ]
    go' ur (Entity fid sf) = object [ "fileUrl" .= ur (FileR fid)
                                    , "fileClass" .= storedFileFAClass sf
                                    , "filename" .= storedFileFullname sf
                                    ]
      
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

putReopenTicketR :: TicketId -> Handler ()
putReopenTicketR tid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  runDB $ do
    insert $ Comment tid (Textarea $ render MsgCloseTicket) uid 0 now now
    update tid [TicketStatus =. OPEN, TicketUpdated =. now]
  redirect MyTasksR

putCloseTicketR :: TicketId -> Handler ()
putCloseTicketR tid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  runDB $ do
    insert $ Comment tid (Textarea $ render MsgCloseTicket) uid 0 now now
    update tid [TicketStatus =. CLOSE, TicketUpdated =. now]
  redirect MyTasksR
