module Model where

import ClassyPrelude.Yesod hiding (Status)
import Database.Persist.Quasi

import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Digest.Pure.MD5 (md5)
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
type Codomain = User
type Speaker = User
type IssueTree = (Issue, Opener, [ChannelTree])
type ChannelTree = (ChannelId, Channel, [(TicketId, Ticket, Codomain)])

-- |
-- Extensions for User
--
userName :: User -> Text
userName u = userFamilyName u <> " " <> userGivenName u

userNameId :: User -> Text
userNameId u = userName u <> "(" <> userIdent u <> ")"

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
issueLimitDatetime = liftM2 day'timeToUTC <$> issueLimitdate <*> issueLimittime

-- |
-- Extensions for Ticket
--
close :: Ticket -> Bool
close Ticket { ticketStatus = CLOSE } = True
close Ticket { ticketStatus = OPEN }  = False

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

