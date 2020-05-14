{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.IdInstances where

import Data.Aeson
import Database.Selda
import Web.Scotty (Parsable (..))

instance ToJSON (ID a) where
  toJSON = toJSON . fromId

instance FromJSON (ID a) where
  parseJSON x = toId <$> parseJSON x

instance Parsable (ID a) where
  parseParam = fmap toId . parseParam
