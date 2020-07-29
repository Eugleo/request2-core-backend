{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.PropertyType where

import Data.Aeson
import Database.Selda

data PropertyType = Comment | Note | Result | General | Detail
  deriving (Show, Read, Eq, Bounded, Enum, Generic, SqlType, FromJSON)

instance ToJSON PropertyType where
  toEncoding = genericToEncoding defaultOptions
