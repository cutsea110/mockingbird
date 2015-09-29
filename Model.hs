module Model where

import Prelude (read)
import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Data.Text as T
import Data.Time

import Model.Fields
import Util
import Settings as Settings

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

-- |
-- Extensions for Issue
--
issueLimitDatetime :: Issue -> Maybe UTCTime
issueLimitDatetime = liftM2 day'timeToUTC <$> issueLimitdate <*> issueLimittime
