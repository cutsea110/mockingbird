module Model where

import ClassyPrelude.Yesod as Prelude hiding (Status, fromList, lookup, foldr)
import Database.Persist.Quasi

import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Digest.Pure.MD5 (md5)
import Data.List (foldr)
import Data.Map (lookup, fromList)
import Data.Text (strip)
import Data.Time

import Model.Fields
import Util

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- |
-- type aliases
--
type Opener = User
type Domain = User
type Codomain = User
type Opponent = User
type Speaker = User
type ChannelMembers = [(Entity Ticket, User)]
type Percentage = Int
type IssueTree = (Issue, Opener, [ChannelTree])
type ChannelTree = (ChannelId, Channel, [(TicketId, Ticket, Codomain)])
type FullEquipedComment = (Issue, Entity Comment, Speaker, Opponent, Maybe [Entity StoredFile], Status)
type FullEquipedIssue = (Entity Issue, Opener, [ChannelTree], (Percentage, Status))
type FullEquipedThread
  = (Entity Issue, Opener, Ticket, Codomain, ChannelMembers, [(Entity Comment, Speaker, Maybe [Entity StoredFile])])
-- |
-- Extensions for User
--
userName :: User -> Text
userName u = userFamilyName u <> " " <> userGivenName u

userNameId :: User -> Text
userNameId u = userName u <> "(" <> userIdent u <> ")"

userGravatarTiny :: User -> Text
userGravatarTiny = gravatarUrl 18 . toGravatarHash . userEmail

userGravatarSmall :: User -> Text
userGravatarSmall = gravatarUrl 36 . toGravatarHash . userEmail

userGravatar :: User -> Text
userGravatar = gravatarUrl 80 . toGravatarHash . userEmail

toGravatarHash :: Text -> Text
toGravatarHash = pack . show . md5 . BL.fromString . unpack . toLower . strip

gravatarUrl :: Int -> Text -> Text
gravatarUrl s h = concat [ "https://secure.gravatar.com/avatar/"
                         , h
                         , "?d=identicon&s="
                         , pack $ show s
                         ]

-- |
-- Extensions for Issue
--
issueLimitDatetime :: Issue -> Maybe UTCTime
issueLimitDatetime = liftM2 day'timeToUTC <$> issueLimitdate <*> issueLimittime'
    where
      issueLimittime' = return . maybe midnight id . issueLimittime

own :: MonadIO m => UserId -> IssueId -> ReaderT SqlBackend m Bool
uid `own` key = do
  issue <- get404 key
  return $ issueOpener issue == uid

involved :: MonadIO m => UserId -> IssueId -> ReaderT SqlBackend m Bool
uid `involved` key = do
  cs <- selectList [ChannelIssue ==. key] []
  let cids = Prelude.map entityKey cs
  c <- count ([TicketChannel <-. cids] ++ ([TicketCodomain ==. uid] ||. [TicketDomain ==. uid]))
  return $ c > 0

issueStatus :: [ChannelTree] -> Status
issueStatus cs = if issueProgress cs == 100 then CLOSE else OPEN
issueProgress :: [ChannelTree] -> Percentage
issueProgress cs
  = div' $ foldr (\s (c, t) -> (if channelTreeStatus s==CLOSE then c+1 else c, t+1)) (0, 0) cs
    where
      div' (num, den) = 100 * num `div` den

channelTreeProgress :: ChannelTree -> Percentage
channelTreeProgress (_, ch, ts)
  = div' $ foldr (\(_, tick, _) (c, t) -> (if ticketStatus tick == CLOSE then c+1 else c, t+1)) (0, 0) ts
    where
      div' (num, den) = 100 * num `div` den

channelTreeStatus :: ChannelTree -> Status
channelTreeStatus (cid, ch, ts) = if pred (==CLOSE) (map (ticketStatus . snd3) ts) then CLOSE else OPEN
  where
    pred = case channelType ch of
      ALL -> all
      ANY -> any

-- |
-- Extensions for Ticket
--
close :: Ticket -> Bool
close Ticket { ticketStatus = CLOSE } = True
close Ticket { ticketStatus = OPEN }  = False

has :: MonadIO m => UserId -> TicketId -> ReaderT SqlBackend m Bool
uid `has` key = do
  t <- get404 key
  return $ uid `elem` [ticketCodomain t, ticketDomain t, ticketAssign t]

-- |
-- Extensions of Channel
--
channelStatus :: MonadIO m => ChannelId -> ReaderT SqlBackend m Status
channelStatus cid = do
  ch <- get404 cid
  case channelType ch of
    ALL -> do
      open <- count [TicketChannel ==. cid, TicketStatus ==. OPEN]
      return $ if open > 0 then OPEN else CLOSE
    ANY -> do
      close <- count [TicketChannel ==. cid, TicketStatus ==. CLOSE]
      return $ if close > 0 then CLOSE else OPEN

joined :: MonadIO m => UserId -> ChannelId -> ReaderT SqlBackend m Bool
uid `joined` key = do
  c <- count ([TicketChannel ==. key] ++ ([TicketCodomain ==. uid] ||. [TicketDomain ==. uid]))
  return $ c > 0

type FAClass = Text

isImageFile :: StoredFile -> Bool
isImageFile sf = Prelude.toLower (storedFileExtension sf) `elem` list
    where
      list = [ ".gif"
             , ".jpeg"
             , ".jpg"
             , ".pct"
             , ".pict"
             , ".png"
             , ".tif"
             , ".tiff"
             ]
isMovieFile :: StoredFile -> Bool
isMovieFile sf = Prelude.toLower (storedFileExtension sf) `elem` list
    where
      list = [ ".avi"
             , ".mov"
             , ".mpeg"
             , ".mpg"
             , ".mp4"
             , ".ogv"
             ]

storedFileFAClass :: StoredFile -> FAClass
storedFileFAClass sf = maybe "file-o" id $ lookup (Prelude.toLower $ storedFileExtension sf) dict
    where
      dict = fromList [ (".agda", "file-code-o")
                      , (".asm", "file-code-o")
                      , (".c", "file-code-o")
                      , (".cpp", "file-code-o")
                      , (".cs", "file-code-o")
                      , (".css", "file-code-o")
                      , (".d", "file-code-o")
                      , (".erl", "file-code-o")
                      , (".go", "file-code-o")
                      , (".hs", "file-code-o")
                      , (".htm", "file-code-o")
                      , (".html", "file-code-o")
                      , (".java", "file-code-o")
                      , (".js", "file-code-o")
                      , (".json", "file-code-o")
                      , (".lhs", "file-code-o")
                      , (".lisp", "file-code-o")
                      , (".lua", "file-code-o")
                      , (".md", "file-code-o")
                      , (".mk", "file-code-o")
                      , (".ml", "file-code-o")
                      , (".php", "file-code-o")
                      , (".pl", "file-code-o")
                      , (".py", "file-code-o")
                      , (".r", "file-code-o")
                      , (".rb", "file-code-o")
                      , (".scala", "file-code-o")
                      , (".scm", "file-code-o")
                      , (".sh", "file-code-o")
                      , (".sql", "file-code-o")
                      , (".xhtml", "file-code-o")
                      , (".xml", "file-code-o")
                        
                      , (".Z", "file-zip-o")
                      , (".bz", "file-zip-o")
                      , (".bz2", "file-zip-o")
                      , (".gz", "file-zip-o")
                      , (".lzh", "file-zip-o")
                      , (".tar", "file-zip-o")
                      , (".tgz", "file-zip-o")
                      , (".xz", "file-zip-o")
                      , (".zip", "file-zip-o")
                        
                      , (".aif", "file-audio-o")
                      , (".aiff", "file-audio-o")
                      , (".au", "file-audio-o")
                      , (".mid", "file-audio-o")
                      , (".midi", "file-audio-o")
                      , (".mp3", "file-audio-o")
                      , (".ra", "file-audio-o")
                      , (".ram", "file-audio-o")
                      , (".wav", "file-audio-o")
                        
                      , (".bmp", "file-image-o")
                      , (".gif", "file-image-o")
                      , (".jpeg", "file-image-o")
                      , (".jpg", "file-image-o")
                      , (".pct", "file-image-o")
                      , (".pict", "file-image-o")
                      , (".png", "file-image-o")
                      , (".tif", "file-image-o")
                      , (".tiff", "file-image-o")
                        
                      , (".avi", "file-movie-o")
                      , (".dcr", "file-movie-o")
                      , (".dir", "file-movie-o")
                      , (".dxr", "file-movie-o")
                      , (".mov", "file-movie-o")
                      , (".mpeg", "file-movie-o")
                      , (".mpg", "file-movie-o")
                      , (".mp4", "file-movie-o")
                      , (".ogv", "file-movie-o")
                      , (".vdo", "file-movie-o")
                      , (".wrl", "file-movie-o")
                        
                      , (".pdf", "file-pdf-o")
                        
                      , (".text", "file-text-o")
                      , (".txt", "file-text-o")
                        
                      , (".doc", "file-word-o")
                      , (".docx", "file-word-o")
                        
                      , (".xls", "file-excel-o")
                      , (".xlsx", "file-excel-o")
                        
                      , (".ppt", "file-powerpoint-o")
                      , (".pptx", "file-powerpoint-o")
                      ]
