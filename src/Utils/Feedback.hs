{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Utils.Feedback where

import Api.Common (failure)
import Control.Monad (when)
import Data.Aeson
import Data.Environment
import Data.Model.DateTime
import Data.Model.User
import Data.Text
import Data.UserInfo
import Database.Common (get)
import qualified Database.Table as Table
import GHC.Generics (Generic)
import Network.HTTP.Client.Conduit (Request (method))
import Network.HTTP.Simple (
    getResponseStatusCode,
    httpNoBody,
    parseRequest,
    setRequestBodyJSON,
 )
import Network.HTTP.Types (forbidden403, serviceUnavailable503)


data Feedback = Feedback {path :: Text, datetime :: DateTime, content :: Text}
    deriving (Show, Eq, Generic, FromJSON)


data SlackBody = SlackBody {username :: Text, text :: Text} deriving (Show, Eq, Generic, ToJSON)


buildSlackRequest :: String -> SlackBody -> EnvAction Request
buildSlackRequest url body = do
    let stdBody = body{username = "Request 2 | Feedback (" <> username body <> ")"}
    req <- envIO (setRequestBodyJSON stdBody <$> parseRequest url)
    return $ req{method = "POST"}


slackUrl :: String
slackUrl = "https://hooks.slack.com/services/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"


sendFeedback :: EnvAction ()
sendFeedback = do
    feedback <- jsonData
    ui <- askUserInfo
    user' <- get Table.users . userId $ ui
    case user' of
        Nothing -> failure "Invalid user" forbidden403
        Just user -> do
            req <-
                buildSlackRequest slackUrl $
                    SlackBody
                        (name user)
                        (content feedback <> "\n_at path: " <> path feedback <> "_")
            response <- envIO $ httpNoBody req
            when (getResponseStatusCode response /= 200) $
                failure "Something went wrong on the Slack side, try again." serviceUnavailable503
