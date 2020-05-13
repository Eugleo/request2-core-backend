{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module UserInfo where

import Data.Aeson
import Data.Text
import Database.Selda (ID, SqlRow)
import GHC.Generics
import Model.Role
import Model.User

data UserInfo
  = UserInfo
      { userId :: ID User,
        apiKey :: Text,
        roles :: [Role]
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON UserInfo where
  toEncoding = genericToEncoding defaultOptions
