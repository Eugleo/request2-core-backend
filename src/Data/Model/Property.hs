{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Model.Property where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Request
import Data.Model.User
import Database.Selda

-- TODO Add private properties (can only be viewed by author)
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
