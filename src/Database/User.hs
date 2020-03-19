{-# LANGUAGE RecordWildCards #-}

module Database.User where

import Crypto
import Data.Maybe (fromMaybe)
import Data.Text
import Database.SQLite.Simple
import DateTime
import Environment
import Model.User
import UserInfo
import WithID (ID)

checkPassword :: ID -> Text -> EnvAction Bool
checkPassword user password = do
  db <- askDB
  res <-
    envIO $ query db "SELECT pw_hash FROM users WHERE user_id = ?" (Only user)
  return $
    case res of
      [(Only hash)] -> checkHash password hash
      _ -> False

login :: Text -> Text -> EnvAction (Maybe UserInfo)
login email password = do
  db <- askDB
  res <-
    envIO $
      query db "SELECT pw_hash, user_id FROM users WHERE email = ?" (Only email)
  case res of
    [(hash, userId)] ->
      if checkHash password hash
        then do
          apiKey <- addApiKey userId
          roles <- getRoles userId
          -- TODO Log error on roles == Nothing
          return . Just $ UserInfo userId apiKey (fromMaybe [] roles)
        else return Nothing
    _ -> return Nothing

addApiKey :: ID -> EnvAction APIKey
addApiKey userId = do
  db <- askDB
  envIO $ do
    apiKey <- newApiKey
    time <- now
    execute
      db
      "INSERT INTO api_keys (api_key, user_id, created) VALUES (?, ?, ?)"
      (apiKey, userId, time)
    return apiKey

logout :: APIKey -> EnvAction ()
logout apiKey = do
  db <- askDB
  envIO $ execute db "DELETE FROM api_keys WHERE api_key = ?" (Only apiKey)

logoutEverywhere :: ID -> EnvAction ()
logoutEverywhere userId = do
  db <- askDB
  envIO $ execute db "DELETE FROM api_keys WHERE user_id = ?" (Only userId)

{- this does not create login credentials, use `setPassword`! -}
createUser :: User -> EnvAction ()
createUser user = do
  db <- askDB
  envIO $
    execute
      db
      "INSERT INTO users (email, name, pw_hash, team_id, roles, created) VALUES (?, ?, ?, ?, ?, ?)"
      user

setPassword :: ID -> Text -> EnvAction ()
setPassword userId password = do
  db <- askDB
  envIO $ do
    pwhash <- newHash $ password
    execute db "UPDATE users SET pw_hash = ? WHERE user_id = ?" (pwhash, userId)

getRoles :: ID -> EnvAction (Maybe [Role])
getRoles user = do
  db <- askDB
  res <-
    envIO $ query db "SELECT roles FROM users WHERE user_id = ?" (Only user)
  case res of
    [[rs]] -> return $ Just rs
    _ -> return Nothing
