{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.TeamWithoutId where

import Data.Aeson
import Data.Text ()
import Database.Selda
import Model.IdInstances ()

data TeamWithoutId = Team {name :: Text, code :: Text, active :: Bool}
  deriving (Show, Eq, Generic, FromJSON, SqlRow)

instance ToJSON TeamWithoutId where
  toEncoding = genericToEncoding defaultOptions
