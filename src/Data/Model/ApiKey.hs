{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.ApiKey where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.User (User)
import Database.Selda


data ApiKey = ApiKey
    { key :: Text,
      userId :: ID User,
      dateCreated :: DateTime
    }
    deriving (Eq, Show, Generic, SqlRow)


instance ToJSON ApiKey where
    toEncoding = genericToEncoding defaultOptions
