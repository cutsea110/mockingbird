module Handler.Thread where

import Import as Import
import Yesod.Form.Bootstrap3

import Model.Fields

getComments :: MonadIO m => IssueId -> ChannelId -> ReaderT SqlBackend m (Issue, User, [(Comment, User)])
getComments key cid = do
  issue <- get404 key
  opener <- get404 $ issueOpener issue
  cs <- selectList [CommentChannel ==. cid] [Desc CommentCreated]
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
    <*  bootstrapSubmit bfs'submit
  where
    bfs'comment = bfs'focus (render MsgComment) (render MsgSimpleAndClarity)
    bfs'submit = BootstrapSubmit (render MsgCommenting) "btn-default" []

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
postCommentR key cid = undefined
