module Model.Fields where

import Prelude
import Yesod
import Data.Time.LocalTime

data Logic = ALL | ANY
           deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Logic"

data Status = OPEN | CLOSE
           deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Status"

instance PathPiece Textarea

instance PathPiece TimeOfDay

  
