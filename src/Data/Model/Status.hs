{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.Status where

import Data.Aeson
import Database.Selda (Generic, SqlType)


data Status = Pending | InProgress | Done | AwaitingInput | Deleted
    deriving (Show, Read, Eq, Bounded, Enum, Generic, SqlType, FromJSON)


instance ToJSON Status where
    toEncoding = genericToEncoding defaultOptions
