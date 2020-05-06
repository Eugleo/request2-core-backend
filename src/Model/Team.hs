{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Team where

import Data.Aeson
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import WithID

data Team = Team {name :: Text, active :: Bool} deriving (Show, Eq, Generic)

instance FromJSON Team

instance ToJSON Team where
  toEncoding = genericToEncoding defaultOptions

instance FromRow (WithID Team) where
  fromRow = WithID <$> field <*> (Team <$> field <*> field)

instance ToRow Team where
  toRow Team {..} = toRow (name, active)
