{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.SecurityToken where

import Data.Model.DateTime
import Database.Selda


data SecurityToken = SecurityToken
    { token :: Text,
      email :: Text,
      validUntil :: DateTime
    }
    deriving (Eq, Show, Generic, SqlRow)
