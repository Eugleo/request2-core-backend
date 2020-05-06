{-# LANGUAGE RecordWildCards #-}

module Database.User where

import Crypto
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text
import Database.PostgreSQL.Simple
import qualified Database.Team as TeamDB
import DateTime
import Environment
import Model.User
import Model.UserDetails (UserDetails (..))
import UserInfo
import WithID (ID, WithID (..))

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
  void . envIO $ execute db "DELETE FROM api_keys WHERE api_key = ?" (Only apiKey)

logoutEverywhere :: ID -> EnvAction ()
logoutEverywhere userId = do
  db <- askDB
  void . envIO $ execute db "DELETE FROM api_keys WHERE user_id = ?" (Only userId)

{- this does not create login credentials, use `setPassword`! -}
create :: User -> Text -> EnvAction (WithID User)
create u@(User {..}) pwHash = do
  db <- askDB
  userID <- envIO $ do
    execute
      db
      "INSERT INTO users (email, name, pw_hash, team_id, roles, created) VALUES (?, ?, ?, ?, ?, ?)"
      (email, name, pwHash, team, roles, created)
    fromIntegral <$> lastInsertRowId db
  return $ WithID userID u

getDetails :: ID -> EnvAction (Maybe UserDetails)
getDetails userId = do
  db <- askDB
  res <-
    envIO $
      query
        db
        "SELECT name, team_id, roles, created FROM users WHERE user_id = ?"
        (Only userId)
  case res of
    [(name, teamId, roles, created)] -> do
      maybeTeam <- TeamDB.get teamId
      case maybeTeam of
        Just (WithID _ team) ->
          return . Just $ UserDetails name roles team created
        _ -> return Nothing
    _ -> return Nothing

setPassword :: ID -> Text -> EnvAction ()
setPassword userId password = do
  db <- askDB
  void . envIO $ do
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
