module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Data.Text as T
import Data.Time (Day, TimeOfDay(..))

import Model.Fields

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

userName :: User -> Text
userName u = if T.null $ userFamilyName u <> userGivenName u
             then "(no name)"
             else userFamilyName u <> " " <> userGivenName u
