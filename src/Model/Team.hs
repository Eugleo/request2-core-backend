{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Team where

import Data.Aeson
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics
import WithID

data Team = Team {name :: Text, active :: Bool}
  deriving (Show, Eq, Generic, ToRow, FromJSON)

instance ToJSON Team where
  toEncoding = genericToEncoding defaultOptions

instance FromRow (WithID Team) where
  fromRow = WithID <$> field <*> (Team <$> field <*> field)
