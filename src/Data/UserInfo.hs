{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.UserInfo where

import Data.Aeson
import Data.Model.Role
import Data.Model.User
import Data.Text
import Database.Selda (ID)
import GHC.Generics
import Utils.Id.IdInstances ()


data UserInfo = UserInfo
    { userId :: ID User,
      apiKey :: Text,
      roles :: [Role]
    }
    deriving (Show, Eq, Generic, FromJSON)


instance ToJSON UserInfo where
    toEncoding = genericToEncoding defaultOptions
