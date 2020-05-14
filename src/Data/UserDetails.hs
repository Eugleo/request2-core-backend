{-# LANGUAGE DeriveGeneric #-}

module Data.UserDetails where

import Data.Aeson
import Data.Model.DateTime (DateTime)
import Data.Model.Role
import Data.Model.Team (Team)
import Data.Text (Text)
import GHC.Generics

data UserDetails
  = UserDetails
      { name :: Text,
        roles :: [Role],
        team :: Team,
        dateCreated :: DateTime
      }
  deriving (Show, Eq, Generic)

instance ToJSON UserDetails where
  toEncoding = genericToEncoding defaultOptions
