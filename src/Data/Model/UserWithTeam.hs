{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Model.UserWithTeam where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Role
import Data.Model.Team
import qualified Data.Model.User as U
import Database.Selda


-- TODO Add password hash
data User = User
    { _id :: ID U.User,
      email :: Text,
      password :: Text,
      name :: Text,
      roles :: [Role],
      teamIds :: [ID Team],
      dateCreated :: DateTime,
      active :: Bool
    }
    deriving (Show, Eq, Generic, FromJSON)


instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions
