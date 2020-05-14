{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.ReqWithProps where

import Data.Aeson
import Data.Model.Request
import Data.PropertyWithoutId
import Database.Selda

data ReqWithProps
  = RWP
      { req :: Request,
        props :: [PropertyWithoutId]
      }
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON ReqWithProps where
  toEncoding = genericToEncoding defaultOptions
