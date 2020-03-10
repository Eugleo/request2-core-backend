{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Announcement where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics
import Time
import WithID

data Announcement
  = Ann
      { title :: String,
        body :: String,
        authorID :: ID,
        dateCreated :: Time,
        dateModified :: Maybe Time,
        active :: Bool
      }
  deriving (Show, Eq, Generic)

instance FromJSON Announcement

instance ToJSON Announcement where
  toEncoding = genericToEncoding defaultOptions

instance FromRow (WithID Announcement) where
  fromRow = WithID <$> field <*> (Ann <$> field <*> field <*> field <*> field <*> field <*> field)

instance ToRow Announcement where
  toRow Ann {..} = toRow (title, body, authorID, dateCreated, dateModified, active)
