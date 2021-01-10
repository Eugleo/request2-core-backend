{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Model.UserWithTeam where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Role
import Data.Model.Team (Team)
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


outerToInner :: User -> U.User
outerToInner User{_id, email, password, name, roles, dateCreated, active} =
    U.User _id email password name roles dateCreated active


instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions
