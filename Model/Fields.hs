module Model.Fields where

import Prelude
import Yesod
import qualified Data.Text as T
import Data.Time
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
  fromPathPiece = Just . read . T.unpack . safe
      where
        safe t = let (h:m:s) = T.splitOn ":" t
                in if null s
                   then T.intercalate ":" [h,m,"00"]
                   else t
  toPathPiece = T.pack . show

instance PathPiece UTCTime where
  fromPathPiece = Just . read . T.unpack
  toPathPiece = T.pack . show
