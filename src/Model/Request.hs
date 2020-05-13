{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Request where

import Data.Aeson
import Database.Selda
import DateTime
import Model.Status
import Model.Team
import Model.User

data Request
  = Request
      { _id :: ID Request,
        authorId :: ID User,
        teamId :: ID Team,
        status :: Status,
        requestType :: Text, -- named `type` in db
        dateCreated :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions
