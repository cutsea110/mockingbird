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

getAssinedTickets :: MonadIO m => UserId ->
                     ReaderT SqlBackend m [(Entity Ticket, (Entity Issue, Entity Channel, [(Ticket, User)]))]
getAssinedTickets uid = do
  ticks <- selectList [TicketAssign ==. uid, TicketStatus ==. OPEN] []
  forM ticks $ \tick@(Entity tid t) -> do
    let cid = ticketChannel t
    ch <- get404 cid
    ts <- selectList [TicketChannel ==. ticketChannel t] []
    tu <- forM ts $ \(Entity _ t) -> do
      u <- get404 $ ticketCodomain t
      return (t, u)
    let key = channelIssue ch
    issue <- get404 key
    return (tick, (Entity key issue, Entity cid ch, tu))

getTasksR :: UserId -> Handler Html
getTasksR uid = do
  Entity _ u <- requireAuth
  now <- liftIO getCurrentTime
  ticks <- runDB $ getAssinedTickets uid
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
    fst3 (x, _, _) = x
    acc = entityVal . fst3 . snd
