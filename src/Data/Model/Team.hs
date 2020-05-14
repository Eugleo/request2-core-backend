{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.Team where

import Data.Aeson
import Data.Text ()
import Database.Selda
import Utils.Id.IdInstances ()

data Team
  = Team
      { _id :: ID Team,
        name :: Text,
        code :: Text,
        active :: Bool
      }
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON Team where
  toEncoding = genericToEncoding defaultOptions
