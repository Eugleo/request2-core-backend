{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Model.Property where

import Data.Aeson
import Data.Aeson.Types
import Data.Model.DateTime
import Data.Model.Request
import Data.Model.User
import qualified Data.Vector as V
import Database.Selda


-- TODO Add private properties (can only be viewed by author)
data Property = Property
    { requestId :: ID Request,
      authorId :: ID User,
      name :: Text,
      value :: Text,
      dateAdded :: DateTime,
      shouldLog :: Bool,
      active :: Bool
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON Property where
    toEncoding = genericToEncoding defaultOptions


parseProperty :: Value -> Parser (Text, Text)
parseProperty = withObject "propertyStub" $ \o -> do
    n <- o .: "name"
    v <- o .: "value"
    return (n, v)


parseProperties :: Value -> Parser [(Text, Text)]
parseProperties = withObject "requestAndProperty" $ \o -> do
    props <- o .: "props"
    mapM parseProperty $ V.toList props