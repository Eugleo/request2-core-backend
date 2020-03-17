{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Property where

import Data.Aeson
import Database.SQLite.Simple
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
        deleted :: Bool
      }
  deriving (Show, Eq, Generic)

instance FromJSON Property

instance ToJSON Property where
  toEncoding = genericToEncoding defaultOptions

instance FromRow (WithID Property) where
  fromRow = WithID <$> field <*> (Property <$> field <*> field <*> field <*> field <*> field <*> field)

instance ToRow Property where
  toRow Property {..} = toRow (requestID, authorID, propertyType, propertyData, dateAdded, deleted)
