{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Config
import Network.HTTP.Types.Status
import UserDB
import UserInfo
import Utils
import Web.Scotty

login config = do
  user <- unpack <$> stringParam "user"
  password <- unpack <$> stringParam "password"
  res <- liftAndCatchIO $ doLogin config user password
  case res of
    Just apikey -> json apikey
    Nothing -> status forbidden403 >> finish

logout config =
  authentized config $ liftAndCatchIO . doLogout config . apiKey

changePassword config ui = do
  oldpass <- unpack <$> stringParam "old"
  newpass <- unpack <$> stringParam "new"
  match <- liftAndCatchIO $ doCheckPassword config (user ui) oldpass
  if match then liftAndCatchIO $ setPassword config (user ui) newpass
  else status forbidden403 >> finish

authentized :: ServerConfig -> (UserInfo -> ActionM ()) -> ActionM ()
authentized c a = do
  apikey <- unpack <$> stringParam "api_key"
  auth <- liftAndCatchIO $ doVerify c apikey
  case auth of
    Just user -> a $ UserInfo user apikey
    Nothing -> status forbidden403 >> finish
