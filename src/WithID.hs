{-# LANGUAGE OverloadedStrings #-}

module WithID where

import Data.Aeson
import Control.Applicative (empty)

type ID = Int

data WithID a = WithID ID a deriving (Show, Eq)

instance FromJSON a => FromJSON (WithID a) where
  parseJSON (Object v) = WithID
    <$> v .: "id"
    <*> v .: "data"
  parseJSON _ = empty

instance ToJSON a => ToJSON (WithID a) where
  toJSON (WithID i a) = object ["id" .= i, "data" .= toJSON a]
