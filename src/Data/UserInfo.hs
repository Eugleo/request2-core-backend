{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.UserInfo where

import Data.Aeson
import Data.Model.Role
import Data.Model.User
import Data.Text
import Database.Selda (ID, SqlRow)
import GHC.Generics


data UserInfo = UserInfo
    { userId :: ID User,
      apiKey :: Text,
      roles :: [Role]
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON UserInfo where
    toEncoding = genericToEncoding defaultOptions
