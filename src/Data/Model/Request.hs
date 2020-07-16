{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.Request where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Status
import Data.Model.Team
import Data.Model.User
import Database.Selda

data Request
  = Request
      { _id :: ID Request,
        name :: Text,
        code :: Text,
        authorId :: ID User,
        teamId :: ID Team,
        assigneeId :: Maybe (ID User),
        status :: Status,
        requestType :: Text, -- named `type` in db
        dateCreated :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions
