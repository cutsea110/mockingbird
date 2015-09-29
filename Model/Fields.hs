module Model.Fields where

import Prelude
import Yesod

data Logic = AND | OR
           deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Logic"

data Status = OPEN | CLOSE
           deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Status"
