{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Model.Ann where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.User
import Database.Selda

data Ann = Ann
  { _id :: ID Ann,
    title :: Text,
    body :: Text,
    authorId :: ID User,
    dateCreated :: DateTime,
    active :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON Ann where
  toEncoding = genericToEncoding defaultOptions
