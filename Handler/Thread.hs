module Handler.Thread where

import Import as Import hiding (Status, last, urlEncode, urlDecode)
import Control.Arrow ((***))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (last)
import Data.Conduit.List (consume)
import Network.HTTP.Base (urlEncode, urlDecode)
import System.Directory
import System.FilePath
import Yesod.Form.Jquery

import Model.Fields (Status(..))
import Util

commentForm jqueryJs uid tid render mv = (,)
                                         <$> (Comment
                                              <$> pure tid
                                              <*> areq textareaField bfs'comment (commentComment <$> mv)
                                              <*> pure uid
                                              <*> pure 0
                                              <*> lift (liftIO getCurrentTime)
                                              <*> lift (liftIO getCurrentTime))
                                         <*>  aopt (filesField jqueryJs) bfs'file Nothing
  where
    bfs'comment = bfs'focus (render MsgComment) (render MsgCommenting)
    bfs'file = bfs' (render MsgAttachFile) (render MsgAttachFile)

getThreadR :: TicketId -> Handler Html
getThreadR tid = do
  uid <- requireAuthId
  render <- getMessageRender
  now <- liftIO getCurrentTime
  attachBtnId <- newIdent
  master <- getYesod
  ((_, w), enc) <- runFormInline $ commentForm (urlJqueryJs master) uid tid render Nothing
  (Entity key issue, opener, comments) <- runDB $ do
    t <- get404 tid
    ch <- get404 $ ticketChannel t
    let key = channelIssue ch
    issue <- get404 key
    op <- get404 $ issueOpener issue
    cs <- selectList [CommentTicket ==. tid] [Asc CommentCreated]
    cs' <- forM cs $ \comment@(Entity cid c) -> do
      u <- get404 $ commentSpeaker c
      mf <- if commentAttached c > 0
            then do
              fs <- selectList [StoredFileComment ==. cid] []
              return (Just fs)
            else return Nothing
      return (comment, u, mf)
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
  master <- getYesod
  ((r, _), _) <- runFormInline $ commentForm (urlJqueryJs master) uid tid render Nothing
  case r of
    FormSuccess (comment, mfis) -> do
      runDB $ do
        t <- get404 tid
        let (you, me) = (if ticketDomain t == uid then ticketCodomain t else ticketDomain t, uid)
            (assign, status) = case turn of
              "YOU" -> (you, ticketStatus t)
              "ME"  -> (me, ticketStatus t)
              "NOBODY" -> (ticketAssign t, CLOSE)
        cid <- insert comment { commentAttached = maybe 0 length mfis }
        update tid [TicketAssign =. assign, TicketStatus =. status]
        let dir = s3dir </> T.unpack (toPathPiece uid) </> T.unpack (toPathPiece cid)
        liftIO $ createDirectoryIfMissing True dir
        whenJust_ mfis $ \fis -> forM_ fis $ \fi -> do
          (sf, lbs) <- toStoredFile uid cid now fi
          fid <- insert sf
          let fp = dir </> T.unpack (toPathPiece fid)
          liftIO $ L.writeFile fp lbs
      redirect $ ThreadR tid
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]

toStoredFile :: MonadResource m => UserId -> CommentId -> UTCTime -> FileInfo -> m (StoredFile, L.ByteString)
toStoredFile uid cid now fi = do
    lbs <- fileContent fi
    return (StoredFile { storedFileComment = cid
                       , storedFileFullname = T.pack filename
                       , storedFileEncodedName = T.pack $ urlEncode filename
                       , storedFileName = T.pack name
                       , storedFileExtension = T.pack ext
                       , storedFileContentType = fileContentType fi
                       , storedFileBytes = L.length lbs
                       , storedFileCreator = uid
                       , storedFileCreated = now
                       }
           , lbs)
    where
      filename = name ++ ext
      name = takeBaseName $ T.unpack $ fileName fi
      ext = takeExtension $ T.unpack $ fileName fi
      fileContent f = L.fromChunks <$> (fileSource f $$ consume)

getFileR :: StoredFileId -> Handler ()
getFileR fid = do
  uid <- requireAuthId
  sf <- runDB $ get404 fid
  let dir = s3dir </> T.unpack (toPathPiece uid) </> T.unpack (toPathPiece $ storedFileComment sf)
      fp = dir </> T.unpack (toPathPiece fid)
  addHeader "Content-Type" $ storedFileContentType sf
  addHeader "Content-Disposition" $ "attachment; filename*=UTF-8''" `T.append` (storedFileEncodedName sf)
  sendFile (TE.encodeUtf8 (storedFileContentType sf)) fp
