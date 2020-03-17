{-# LANGUAGE RecordWildCards #-}

module Database.User where

import Crypto
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
    envIO $ query db "SELECT pw_hash FROM Users WHERE user_id = ?" (Only user)
  return $
    case res of
      [(Only hash)] -> checkHash password hash
      _ -> False

login :: Text -> Text -> EnvAction (Maybe (ID, APIKey))
login email password = do
  db <- askDB
  res <-
    envIO $
      query db "SELECT pw_hash, user_id FROM Users WHERE email = ?" (Only email)
  case res of
    [(hash, userId)] ->
      if checkHash password hash
        then envIO $ do
          apiKey <- newApiKey
          time <- now
          execute
            db
            "INSERT INTO ApiKeys (api_key, user_id, date_created) VALUES (?, ?, ?)"
            (apiKey, userId, time)
          return $ Just (userId, apiKey)
        else return Nothing
    _ -> return Nothing

logout :: APIKey -> EnvAction ()
logout apiKey = do
  db <- askDB
  envIO $ execute db "DELETE FROM ApiKeys WHERE api_key = ?" (Only apiKey)

logoutEverywhere :: ID -> EnvAction ()
logoutEverywhere userId = do
  db <- askDB
  envIO $ execute db "DELETE FROM ApiKeys WHERE user_id = ?" (Only userId)

{- this does not create login credentials, use `setPassword`! -}
createUser :: User -> EnvAction ()
createUser User {..} = do
  db <- askDB
  envIO $
    execute
      db
      "INSERT INTO Users (email, name, pw_hash, team_id, roles) VALUES (?, ?, '-', ?, ?)"
      (email, name, team, rolesToString roles)

setPassword :: ID -> Text -> EnvAction ()
setPassword userId password = do
  db <- askDB
  envIO $ do
    pwhash <- newHash $ password
    execute db "UPDATE Users SET pw_hash = ? WHERE user_id = ?" (pwhash, userId)

getRoles :: ID -> EnvAction (Maybe [Role])
getRoles user = do
  db <- askDB
  res <-
    envIO $ query db "SELECT roles FROM Users WHERE user_id = ?" (Only user)
  case res of
    [[rolesStr]] -> return . Just $ stringToRoles rolesStr
    _ -> return Nothing
