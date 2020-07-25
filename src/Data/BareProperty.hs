{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.BareProperty where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.User
import Database.Selda

data BareProperty
  = Property
      { authorId :: ID User,
        propertyPath :: Text,
        propertyData :: Text,
        dateAdded :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON BareProperty where
  toEncoding = genericToEncoding defaultOptions