{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.TeamWithoutId where

import Data.Aeson
import Data.Text ()
import Database.Selda


data TeamWithoutId = Team {name :: Text, code :: Text, active :: Bool}
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON TeamWithoutId where
    toEncoding = genericToEncoding defaultOptions
