{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Config
import qualified Database.User as DB
import Network.HTTP.Types.Status
import UserInfo (UserInfo (..))
import qualified UserInfo as U
import Utils
import Web.Scotty

login :: ServerConfig -> ActionM ()
login config = do
  user <- unpack <$> stringParam "user"
  password <- unpack <$> stringParam "password"
  res <- liftAndCatchIO $ DB.login config user password
  case res of
    Just apikey -> json apikey
    Nothing -> status forbidden403 >> finish

logout :: ServerConfig -> ActionM ()
logout config =
  authentized config $ liftAndCatchIO . DB.logout config . U.apiKey

changePassword :: ServerConfig -> UserInfo -> ActionM ()
changePassword config ui = do
  oldpass <- unpack <$> stringParam "old"
  newpass <- unpack <$> stringParam "new"
  match <- liftAndCatchIO $ DB.checkPassword config (U.id ui) oldpass
  if match
    then liftAndCatchIO $ DB.setPassword config (U.id ui) newpass
    else status forbidden403 >> finish

authentized :: ServerConfig -> (UserInfo -> ActionM ()) -> ActionM ()
authentized c f = do
  apikey <- unpack <$> stringParam "api_key"
  auth <- liftAndCatchIO $ DB.verify c apikey
  case auth of
    Just u -> f $ UserInfo u apikey
    Nothing -> status forbidden403 >> finish
