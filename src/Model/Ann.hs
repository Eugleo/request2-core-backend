{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Ann where

import Data.Aeson
import Database.Selda
import DateTime
import Model.User

data Ann
  = Ann
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
