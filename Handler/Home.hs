module Handler.Home where

import Import
import Model.Fields

getMyTasksR :: Handler Html
getMyTasksR = do
  uid <- requireAuthId
  getTasksR uid

getTimelineR :: UserId -> Handler Html
getTimelineR _ = do
  Entity _ u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgTimelineOf u
    $(widgetFile "timeline")

type AssignedTicket = (Entity Ticket, (Issue, Opener, Codomain), Maybe (Entity Comment, Speaker))

getAssignedTickets :: MonadIO m => UserId -> ReaderT SqlBackend m [AssignedTicket]
getAssignedTickets uid = do
  ticks <- selectList [TicketAssign ==. uid, TicketStatus ==. OPEN] []
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
