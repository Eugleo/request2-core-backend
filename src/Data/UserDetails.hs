{-# LANGUAGE DeriveGeneric #-}

module Data.UserDetails where

import Data.Aeson
import Data.Model.DateTime (DateTime)
import Data.Model.Role
import Data.Model.Team (Team)
import Data.Model.User (User)
import Database.Selda


data UserDetails = UserDetails
    { _id :: ID User,
      name :: Text,
      email :: Text,
      roles :: [Role],
      teams :: [Team],
      dateCreated :: DateTime,
      telephone :: Text,
      room :: Text,
      active :: Bool
    }
    deriving (Show, Eq, Generic)


instance ToJSON UserDetails where
    toEncoding = genericToEncoding defaultOptions
