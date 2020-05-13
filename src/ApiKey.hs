{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ApiKey where

import Data.Aeson
import Database.Selda
import DateTime
import Model.User (User)

data ApiKey
  = ApiKey
      { key :: Text,
        userId :: ID User,
        dateCreated :: DateTime
      }
  deriving (Eq, Show, Generic, SqlRow)

instance ToJSON ApiKey where
  toEncoding = genericToEncoding defaultOptions
