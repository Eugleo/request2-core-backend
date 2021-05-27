{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.UserWithoutId where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Role
import Database.Selda


-- TODO Remove password hash
data UserWithoutId = User
    { email :: Text,
      password :: Text,
      name :: Text,
      roles :: [Role],
      dateCreated :: DateTime,
      active :: Bool,
      telephone :: Text,
      room :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON UserWithoutId where
    toEncoding = genericToEncoding defaultOptions
