{-# LANGUAGE DeriveGeneric #-}

module Model.UserDetails where

import Data.Aeson
import Data.Text (Text)
import DateTime (DateTime)
import GHC.Generics
import Model.Team (Team)
import Model.User (Role)

data UserDetails
  = UserDetails
      { name :: Text,
        roles :: [Role],
        team :: Team,
        created :: DateTime
      }
  deriving (Show, Eq, Generic)

instance ToJSON UserDetails where
  toEncoding = genericToEncoding defaultOptions
