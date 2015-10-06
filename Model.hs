module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

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
-- Extensions for User
--
userName :: User -> Text
userName u = userFamilyName u <> " " <> userGivenName u

userNameId :: User -> Text
userNameId u = userName u <> "(" <> userIdent u <> ")"

-- |
-- Extensions for Issue
--
issueLimitDatetime :: Issue -> Maybe UTCTime
issueLimitDatetime = liftM2 day'timeToUTC <$> issueLimitdate <*> issueLimittime
