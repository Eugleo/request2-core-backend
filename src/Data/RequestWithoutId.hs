{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.RequestWithoutId where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Status
import Data.Model.Team
import Data.Model.User
import Database.Selda


data RequestWithoutId = Request
    { name :: Text,
      authorId :: ID User,
      teamId :: ID Team,
      status :: Status,
      requestType :: Text, -- named `type` in db
      dateCreated :: DateTime
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON RequestWithoutId where
    toEncoding = genericToEncoding defaultOptions
