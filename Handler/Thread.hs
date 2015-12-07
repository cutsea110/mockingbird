module Handler.Thread where

import Import as Import hiding (Status)

import Model.Fields (Status(..))
import Util

commentForm uid tid render mv = Comment
                                <$> pure tid
                                <*> areq textareaField bfs'comment (commentComment <$> mv)
                                <*> pure uid
                                <*> pure False
                                <*> lift (liftIO getCurrentTime)
                                <*> lift (liftIO getCurrentTime)
                                <*  aopt filesField bfs'file Nothing
  where
    bfs'comment = bfs'focus (render MsgComment) (render MsgCommenting)
    bfs'file = bfs' (render MsgAttachFile) (render MsgAttachFile)

getThreadR :: TicketId -> Handler Html
getThreadR tid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  ((_, w), enc) <- runFormInline $ commentForm uid tid render Nothing
  (Entity key issue, opener, comments) <- runDB $ do
    t <- get404 tid
    ch <- get404 $ ticketChannel t
    let key = channelIssue ch
    issue <- get404 key
    op <- get404 $ issueOpener issue
    cs <- selectList [CommentTicket ==. tid] [Asc CommentCreated]
    cs' <- forM cs $ \comment@(Entity cid c) -> do
      u <- get404 $ commentSpeaker c
      return (comment, u)
    return (Entity key issue, op, cs')
  let createdBefore = (issueCreated issue) `beforeFrom` now
  defaultLayout $ do
    setTitleI MsgThread
    $(widgetFile "thread")

postThreadR :: TicketId -> Handler ()
postThreadR tid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  Just turn <- lookupPostParam "turn"
  ((r, _), _) <- runFormInline $ commentForm uid tid render Nothing
  case r of
    FormSuccess comment -> do
      runDB $ do
        t <- get404 tid
        let (you, me) = (if ticketDomain t == uid then ticketCodomain t else ticketDomain t, uid)
            (assign, status) = case turn of
              "YOU" -> (you, ticketStatus t)
              "ME"  -> (me, ticketStatus t)
              "NOBODY" -> (ticketAssign t, CLOSE)
        insert comment
        update tid [TicketAssign =. assign, TicketStatus =. status]
      redirect $ ThreadR tid
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
