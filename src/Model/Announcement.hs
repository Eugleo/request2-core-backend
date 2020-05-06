{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Announcement where

import Data.Aeson
import Database.PostgreSQL.Simple (FromRow, ToRow)
import DateTime
import GHC.Generics
import WithID

data Announcement = Ann
  { title :: String,
    body :: String,
    authorID :: ID,
    created :: DateTime,
    active :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, FromRow, ToRow)

instance ToJSON Announcement where
  toEncoding = genericToEncoding defaultOptions
