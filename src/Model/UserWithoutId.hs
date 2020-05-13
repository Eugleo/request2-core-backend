{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.UserWithoutId where

import Data.Aeson
import Database.Selda
import DateTime
import Model.Role
import Model.Team

-- TODO Add password hash
data UserWithoutId
  = User
      { email :: Text,
        password :: Text,
        name :: Text,
        roles :: [Role],
        teamId :: ID Team,
        dateCreated :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON UserWithoutId where
  toEncoding = genericToEncoding defaultOptions
