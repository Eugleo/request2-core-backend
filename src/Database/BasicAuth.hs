{-# LANGUAGE OverloadedStrings #-}

module Database.BasicAuth where

import Database.SQLite.Simple
import Model.User (stringToRoles)
import UserInfo

findApiKeyUser :: Connection -> APIKey -> IO (Maybe UserInfo)
findApiKeyUser db key = do
  res <-
    query
      db
      "SELECT users.user_id, users.roles \
      \ FROM api_keys JOIN users ON api_keys.user_id=users.user_id \
      \ WHERE api_key = ?"
      (Only key)
  case res of
    [(user, rolesStr)] ->
      return . Just $ UserInfo user key (stringToRoles rolesStr)
    _ -> return Nothing
