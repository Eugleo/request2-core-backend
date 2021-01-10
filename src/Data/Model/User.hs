{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Model.User where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Role
import Database.Selda
import Utils.Id.IdInstances ()


-- TODO Add password hash
data User = User
    { _id :: ID User,
      email :: Text,
      password :: Text,
      name :: Text,
      roles :: [Role],
      dateCreated :: DateTime,
      active :: Bool
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions
