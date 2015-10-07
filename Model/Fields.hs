module Model.Fields where

import Prelude
import Yesod
import Data.Text
import Data.Time.LocalTime

data Logic = ALL | ANY
           deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Logic"

data Status = OPEN | CLOSE
           deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Status"

instance PathPiece Textarea where
  fromPathPiece = Just . Textarea
  toPathPiece = unTextarea

instance PathPiece TimeOfDay where
  fromPathPiece = Just . read . unpack
  toPathPiece = pack . show
