module Handler.Home where

import Import
import Model.Fields

getTimelineR :: UserId -> Handler Html
getTimelineR _ = do
  Entity _ u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgTimelineOf u
    $(widgetFile "timeline")

getAssinedTickets :: MonadIO m => UserId ->
                     ReaderT SqlBackend m [(Entity Ticket, (Issue, Channel, [(Ticket, User)]))]
getAssinedTickets uid = do
  ticks <- selectList [TicketAssign ==. uid, TicketStatus ==. OPEN] []
  forM ticks $ \tick@(Entity tid t) -> do
    ch <- get404 $ ticketChannel t
    ts <- selectList [TicketChannel ==. ticketChannel t] []
    tu <- forM ts $ \(Entity _ t) -> do
      u <- get404 $ ticketCodomain t
      return (t, u)
    issue <- get404 $ channelIssue ch
    return (tick, (issue, ch, tu))

getTaskR :: UserId -> Handler Html
getTaskR uid = do
  Entity _ u <- requireAuth
  now <- liftIO getCurrentTime
  tickets <- runDB $ getAssinedTickets uid
  defaultLayout $ do
    setTitleI $ MsgTasksOf u
    $(widgetFile "tasks")
