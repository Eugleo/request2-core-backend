{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Team where

import Data.Aeson
import Data.Text ()
import Database.Selda
import Model.IdInstances ()

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
