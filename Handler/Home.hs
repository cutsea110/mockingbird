module Handler.Home where

import Import hiding (Status)
import Data.Text as T (append)
import Database.Persist.Sql

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

commentsPerPage :: Int
commentsPerPage = 50

getComments :: MonadIO m =>
               UserId -> Maybe CommentId -> ReaderT SqlBackend m [(Issue, Entity Comment, Speaker, Maybe [Entity StoredFile], Status)]
getComments uid mcid = do
  ts <- selectList ([TicketDomain ==. uid] ||. [TicketCodomain ==. uid] ||. [TicketAssign ==. uid]) []
  cs <- selectList ([CommentTicket <-. map entityKey ts] ++ before) [Desc CommentCreated, LimitTo commentsPerPage]
  forM cs $ \comment@(Entity cid c) -> do
    u <- get404 $ commentSpeaker c
    mf <- if commentAttached c > 0
          then do
            fs <- selectList [StoredFileComment ==. cid] []
            return (Just fs)
          else return Nothing
    t <- get404 $ commentTicket c
    ch <- get404 $ ticketChannel t
    i <- get404 $ channelIssue ch
    status <- channelStatus $ ticketChannel t
    return (i, comment, u, mf, status)
  where
    before = maybe [] (\x -> [CommentId <. x]) mcid

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
    go cb ur mr (issue, Entity cid' com, spkr, msf, st)
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
               , "comment" .= toJSON (commentComment com)
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

postSearchR :: Handler Html
postSearchR = do
  uid <- requireAuthId
  Just q <- lookupPostParam "q"
  rs <- searcher uid q
  defaultLayout $ do
    setTitleI $ MsgSearchResult q
    $(widgetFile "search-result")

searcher :: UserId -> Text -> Handler [Either (Entity Issue) (Entity Comment)]
searcher uid q = do
  let q' = "%" `T.append` q `T.append` "%"
  runDB $ do
    is <- issues uid q
    cs <- comments uid q
    return (map Left is ++ map Right cs)
    
issues :: MonadIO m => UserId -> Text -> ReaderT SqlBackend m [Entity Issue]
issues uid q = rawSql sql [toPersistValue uid, toPersistValue uid, toPersistValue q', toPersistValue q']
    where
      q' = "%" `T.append` q `T.append` "%"
      sql :: Sql
      sql = "SELECT \
                DISTINCT ?? \
             FROM \
                issue, \
                channel, \
                ticket \
             WHERE \
                   issue.id=channel.issue \
               AND channel.id=ticket.channel \
               AND (ticket.domain=? OR ticket.codomain=?) \
               AND (issue.subject like ? OR issue.description like ?)"

comments :: MonadIO m => UserId -> Text -> ReaderT SqlBackend m [Entity Comment]
comments uid q = rawSql sql [toPersistValue uid, toPersistValue uid, toPersistValue q', toPersistValue q']
    where
      q' = "%" `T.append` q `T.append` "%"
      sql :: Sql
      sql = "WITH ts AS \
             (SELECT DISTINCT id \
              FROM ticket \
              WHERE channel IN (SELECT DISTINCT channel.id \
                                FROM channel, \
                                     ticket \
                                WHERE \
                                      channel.id=ticket.channel \
                                  AND (ticket.domain=? OR ticket.codomain=?))) \
             SELECT \
                 DISTINCT ?? \
             FROM \
                comment LEFT OUTER JOIN stored_file ON comment.id=stored_file.comment \
             WHERE \
                   (comment.comment LIKE ? OR stored_file.fullname LIKE ?) \
               AND comment.ticket IN (SELECT id FROM ts)"
