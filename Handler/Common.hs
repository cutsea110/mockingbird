-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

toFullEquipedIssue :: MonadIO m => [Entity Issue] -> ReaderT SqlBackend m [FullEquipedIssue]
toFullEquipedIssue is = do
  forM is $ \issue@(Entity iid i) -> do
    opener <- get404 (issueOpener i)
    cs <- selectList [ChannelIssue ==. iid] []
    chans <- forM cs $ \(Entity cid c) -> do
      ts <- selectList [TicketChannel ==. cid] []
      tu <- forM ts $ \(Entity tid t) -> do
        u <- get404 (ticketCodomain t)
        return (tid, t, u)
      return (cid, c, tu)
    return (issue, opener, chans)

toFullEquipedComments :: MonadIO m => [Entity Comment] -> ReaderT SqlBackend m [FullEquipedComment]
toFullEquipedComments cs = do
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
  

