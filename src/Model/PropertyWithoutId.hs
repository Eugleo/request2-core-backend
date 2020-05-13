{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.PropertyWithoutId where

import Data.Aeson
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.Selda
import DateTime
import Model.Request
import Model.User

data PropertyWithoutId
  = Property
      { requestId :: ID Request,
        authorId :: ID User,
        propertType :: Text,
        propertyData :: Text,
        dateAdded :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON PropertyWithoutId where
  toEncoding = genericToEncoding defaultOptions
