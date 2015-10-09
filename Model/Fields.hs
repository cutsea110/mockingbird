{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Fields where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

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
  fromPathPiece = Just . read . T.unpack . sanitize
      where
        sanitize t = let (h:m:s) = T.splitOn ":" t
                     in if null s
                        then T.intercalate ":" [h,m,"00"]
                        else t
  toPathPiece = T.pack . show

instance PathPiece UTCTime where
  fromPathPiece = Just . read . T.unpack
  toPathPiece = T.pack . show

class FromText a where
  fromText :: Text -> a

class ToText a where
  toText :: a -> Text

instance FromText Logic where
  fromText = read . T.unpack
