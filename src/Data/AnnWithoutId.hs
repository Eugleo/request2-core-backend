{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.AnnWithoutId where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.User
import Database.Selda

data AnnWithoutId
  = Ann
      { title :: Text,
        body :: Text,
        authorId :: ID User,
        dateCreated :: DateTime,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON AnnWithoutId where
  toEncoding = genericToEncoding defaultOptions
