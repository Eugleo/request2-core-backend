{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.User where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import DateTime
import GHC.Generics
import Text.Read (readMaybe)
import WithID

data Role = Client | Operator | Admin deriving (Show, Read, Eq, Ord)

instance FromJSON Role where
  parseJSON = withText "Role field" $ \v ->
    case readMaybe $ T.unpack v of
      Just role -> return role
      Nothing -> prependFailure "Can't parse role, " (fail $ "incorrect argument: " ++ show v)

instance ToJSON Role where
  toJSON = String . T.pack . show

instance FromField [Role] where
  fromField f mdata = case mdata of
    Just txt -> return . mapMaybe readMaybe . words . B.unpack $ txt
    _ -> returnError Incompatible f "Expected Requested | WIP | Done"

instance ToField [Role] where
  toField = toField . unwords . map show

-- TODO Add password hash
data User
  = User
      { email :: Text,
        name :: Text,
        roles :: [Role],
        team :: ID,
        created :: DateTime
      }
  deriving (Show, Eq, Generic)

instance FromJSON User

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions

instance FromRow (WithID User) where
  fromRow = WithID <$> field <*> (User <$> field <*> field <*> field <*> field <*> field)
