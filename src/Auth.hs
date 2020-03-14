{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Auth where

import qualified Database.User as DB
--import qualified UserInfo as U
import Utils
import Web.Scotty
import Environment

login :: EnvAction ()
login = do
  user <- unpack <$> lift (stringParam "email")
  password <- unpack <$> lift (stringParam "password")
  res <- DB.login user password
  case res of
    Just (_, apikey) -> lift $ json apikey
    Nothing -> envForbidden

{-
logout :: ServerConfig -> ActionM ()
logout config =
  undefined config $ liftAndCatchIO . DB.logout config . U.apiKey

changePassword :: ServerConfig -> U.UserInfo -> ActionM ()
changePassword config ui = do
  oldpass <- unpack <$> stringParam "old"
  newpass <- unpack <$> stringParam "new"
  match <- liftAndCatchIO $ DB.checkPassword config (U.userID ui) oldpass
  if match
    then liftAndCatchIO $ DB.setPassword config (U.userID ui) newpass
    else finishForbidden
-}
