{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Property where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import DateTime
import GHC.Generics
import WithID

data Property
  = Property
      { requestID :: ID,
        authorID :: ID,
        propertyType :: String, -- named `type` in db
        propertyData :: String, -- named `data` in db
        dateAdded :: DateTime,
        enabled :: Bool
      }
  deriving (Show, Eq, Generic)

instance FromJSON Property

instance ToJSON Property where
  toEncoding = genericToEncoding defaultOptions

instance FromRow (WithID Property) where
  fromRow = WithID <$> field <*> (Property <$> field <*> field <*> field <*> field <*> field <*> field)

instance ToRow Property
