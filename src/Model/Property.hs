{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Property where

import Data.Aeson
import Database.Selda
import DateTime
import Model.Request
import Model.User

data Property
  = Property
      { _id :: ID Property,
        requestId :: ID Request,
        authorId :: ID User,
        propertyType :: Text,
        propertyData :: Text,
        dateAdded :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON Property where
  toEncoding = genericToEncoding defaultOptions
