{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.FileDesc where

import Data.Aeson
import Data.Text
import Database.Selda (Generic)


data FileDesc = FileDesc
    { hash :: Text,
      name :: Text,
      mime :: Text
    }
    deriving (Show, Eq, Generic, FromJSON)


instance ToJSON FileDesc where
    toEncoding = genericToEncoding defaultOptions
