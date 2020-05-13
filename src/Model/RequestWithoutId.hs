{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.RequestWithoutId where

import Data.Aeson
import Database.Selda
import DateTime
import Model.Status
import Model.Team
import Model.User

data RequestWithoutId
  = Request
      { authorId :: ID User,
        teamId :: ID Team,
        status :: Status,
        requestType :: Text, -- named `type` in db
        dateCreated :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON RequestWithoutId where
  toEncoding = genericToEncoding defaultOptions
