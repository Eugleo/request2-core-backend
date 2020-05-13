{-# LANGUAGE DeriveGeneric #-}

module Model.UserDetails where

import Data.Aeson
import Data.Text (Text)
import DateTime (DateTime)
import GHC.Generics
import Model.Role
import Model.Team (Team)

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
