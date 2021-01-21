{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Model.Request where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Model.DateTime
import Data.Model.Status
import Data.Model.Team
import Data.Model.User
import Database.Selda


data Request = Request
    { _id :: ID Request,
      title :: Text,
      authorId :: ID User,
      teamId :: ID Team,
      status :: Status,
      requestType :: Text,
      dateCreated :: DateTime
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)


instance ToJSON Request where
    toEncoding = genericToEncoding defaultOptions


parseRequestCreation :: Value -> Parser (Text, ID Team, Text)
parseRequestCreation = withObject "request" $ \o -> do
    title <- o .: "title"
    teamId <- o .: "teamId"
    requestType <- o .: "requestType"
    return (title, teamId, requestType)


parseRequestEdit :: Value -> Parser (Text, ID Team)
parseRequestEdit = withObject "request" $ \o -> do
    title <- o .: "title"
    teamId <- o .: "teamId"
    return (title, teamId)


parseRequestId :: Value -> Parser (ID Request)
parseRequestId = withObject "requestWithProperties" $ \o -> do
    req <- o .: "request"
    req .: "_id"