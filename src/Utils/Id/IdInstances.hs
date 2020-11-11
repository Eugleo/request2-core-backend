{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils.Id.IdInstances where

import Data.Aeson
import Database.Selda
import Web.Scotty (Parsable (..))


instance ToJSON (ID a) where
    toJSON = toJSON . fromId


instance FromJSON (ID a) where
    parseJSON x = toId <$> parseJSON x


instance Parsable (ID a) where
    parseParam = fmap toId . parseParam


instance Read (ID a) where
    readsPrec = readsPrec
