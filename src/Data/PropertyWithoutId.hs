{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.PropertyWithoutId where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Request
import Data.Model.User
import Database.Selda

data PropertyWithoutId
  = Property
      { requestId :: ID Request,
        authorId :: ID User,
        propertyPath :: Text,
        propertyData :: Text,
        dateAdded :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON PropertyWithoutId where
  toEncoding = genericToEncoding defaultOptions
