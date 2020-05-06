{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module UserInfo where

import Data.Aeson
import Data.Text
import GHC.Generics
import Model.User (Role)
import WithID (ID)

type APIKey = Text

data UserInfo = UserInfo
  { userID :: ID,
    apiKey :: APIKey,
    roles :: [Role]
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserInfo

instance ToJSON UserInfo where
  toEncoding = genericToEncoding defaultOptions
