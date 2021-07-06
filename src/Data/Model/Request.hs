{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.Request where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Status
import Data.Model.Team
import Data.Model.User
import Database.Selda


data Request = Request
    { _id :: ID Request,
      title :: Text,
      authorId :: ID User,
      teamId :: ID Team,
      status :: Status,
      requestType :: Text,
      dateCreated :: DateTime
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON Request where
    toEncoding = genericToEncoding defaultOptions
