{-# LANGUAGE OverloadedStrings #-}

module Database.BasicAuth where

import Database.PostgreSQL.Simple
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
    [(user, rs)] -> return . Just $ UserInfo user key rs
    _ -> return Nothing
