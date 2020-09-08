{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.PropertyType where

import Data.Aeson
import Database.Selda

--TODO explain the differene between Comment&Note and General&Detail
data PropertyType = Comment | Note | Result | General | Detail | File | ResultFile
  deriving (Show, Read, Eq, Bounded, Enum, Generic, SqlType, FromJSON)

instance ToJSON PropertyType where
  toEncoding = genericToEncoding defaultOptions
