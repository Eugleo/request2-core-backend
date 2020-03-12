{-# LANGUAGE DeriveGeneric #-}

module WithID where

import Data.Aeson
import GHC.Generics

type ID = Int

data WithID a = WithID ID a deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (WithID a)

instance ToJSON a => ToJSON (WithID a) where
  toEncoding = genericToEncoding defaultOptions
