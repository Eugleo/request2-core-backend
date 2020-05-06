{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Team where

import Data.Aeson
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics

data Team = Team
  { name :: Text,
    active :: Bool
  }
  deriving (Show, Eq, Generic, ToRow, FromJSON)

instance ToJSON Team where
  toEncoding = genericToEncoding defaultOptions

instance FromRow Team where
  fromRow = Team <$> field <*> field
