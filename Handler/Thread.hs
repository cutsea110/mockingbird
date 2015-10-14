module Handler.Thread where

import Import as Import
import Yesod.Form.Bootstrap3

import Model.Fields

getComments :: MonadIO m => IssueId -> ChannelId -> ReaderT SqlBackend m (Issue, User, [(Comment, User)])
getComments key cid = do
  issue <- get404 key
  opener <- get404 $ issueOpener issue
  cs <- selectList [CommentChannel ==. cid] [Asc CommentCreated]
  comments <- forM cs $ \(Entity _ c) -> do
    u <- get404 $ commentSpeaker c
    return (c, u)
  return (issue, opener, comments)

commentForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
               UserId -> ChannelId -> (AppMessage -> Text) -> Maybe Comment -> AForm m Comment
commentForm uid cid render mv
  = Comment
    <$> pure cid
    <*> areq textareaField bfs'comment (commentComment <$> mv)
    <*> pure uid
    <*> lift (liftIO getCurrentTime)
    <*> lift (liftIO getCurrentTime)
  where
    bfs'comment = bfs'focus (render MsgComment) (render MsgSimpleAndClarity)

getThreadR :: IssueId -> ChannelId -> Handler Html
getThreadR key cid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  (issue, opener, comments) <- runDB $ getComments key cid
  let createdBefore = (issueCreated issue) `beforeFrom` now
  ((_, w), enc) <- runFormInline $ commentForm uid cid render Nothing
  defaultLayout $ do
    setTitleI MsgThread
    $(widgetFile "thread")

postCommentR :: IssueId -> ChannelId -> Handler ()
postCommentR key cid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  Just turn <- lookupPostParam "turn"
  ((r, _), _) <- runForm $ commentForm uid cid render Nothing
  case r of
    FormSuccess comment -> do
      runDB $ do
        insert comment
        Just (Entity tid t)
            <- selectFirst ([TicketChannel ==. cid] ++ [TicketDomain ==. uid] ||. [TicketCodomain ==. uid]) []
        when (ticketDomain t /= ticketCodomain t) $ do
          let (you, me) = (if ticketDomain t == uid then ticketCodomain t else ticketDomain t, uid)
          update tid [TicketAssign =. if turn == "YOU" then you else me]
      redirect $ THREAD $ ThreadR key cid
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
