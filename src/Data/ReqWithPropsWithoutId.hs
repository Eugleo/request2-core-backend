{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.ReqWithPropsWithoutId where

import Data.Aeson
import Data.PropertyWithoutId
import Data.RequestWithoutId (RequestWithoutId)
import Database.Selda

data ReqWithPropsWithoutId
  = RWP
      { req :: RequestWithoutId,
        props :: [PropertyWithoutId]
      }
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON ReqWithPropsWithoutId where
  toEncoding = genericToEncoding defaultOptions
