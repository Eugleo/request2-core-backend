{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.User where

import Data.Aeson
import Database.Selda
import DateTime
import Model.Role
import Model.Team

-- TODO Add password hash
data User
  = User
      { _id :: ID User,
        email :: Text,
        password :: Text,
        name :: Text,
        roles :: [Role],
        teamId :: ID Team,
        dateCreated :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions
