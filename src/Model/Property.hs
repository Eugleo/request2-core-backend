{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Property where

import Data.Aeson
import Database.PostgreSQL.Simple (FromRow, ToRow)
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
  deriving (Show, Eq, Generic, FromJSON, FromRow, ToRow)

instance ToJSON Property where
  toEncoding = genericToEncoding defaultOptions
