{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.ReqWithPropsWithoutId where

import Data.Aeson
import Data.BareProperty
import Data.RequestWithoutId (RequestWithoutId)
import Database.Selda


data ReqWithPropsWithoutId = RWP
    { req :: RequestWithoutId,
      props :: [BareProperty]
    }
    deriving (Show, Eq, Generic, FromJSON)


instance ToJSON ReqWithPropsWithoutId where
    toEncoding = genericToEncoding defaultOptions
