{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.UserWithoutIdWithTeam where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Role
import Data.Model.Team
import Database.Selda


-- TODO Remove password hash
data UserWithoutId = User
    { email :: Text,
      password :: Text,
      name :: Text,
      roles :: [Role],
      teamIds :: [ID Team],
      dateCreated :: DateTime,
      active :: Bool,
      telephone :: Text,
      room :: Text
    }
    deriving (Show, Eq, Generic, FromJSON)


instance ToJSON UserWithoutId where
    toEncoding = genericToEncoding defaultOptions
