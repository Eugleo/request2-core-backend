{-# LANGUAGE OverloadedStrings #-}

module UserDB where

import Config
import Crypto
import Data.UnixTime (getUnixTime, utSeconds)
import Database.SQLite.Simple
import Foreign.C.Types (CTime(..))

dbBracket :: ServerConfig -> (Connection -> IO a) -> IO a
dbBracket c = withConnection (dbPath c)

-- TODO separate this?
createDB c =
  dbBracket c $ \db -> do
    execute_
      db
      "CREATE TABLE IF NOT EXISTS users (user TEXT PRIMARY KEY, pwhash TEXT NOT NULL)"
    execute_
      db
      "CREATE TABLE IF NOT EXISTS apikeys (apikey TEXT PRIMARY KEY, user TEXT NOT NULL, created BIGINT NOT NULL)"

verifyPassword c user password err f =
  dbBracket c $ \db ->
    withTransaction db $ do
      res <-
        query
          db
          "SELECT pwhash FROM users WHERE user = ?"
          (Only (user :: String)) :: IO [[String]]
      case res of
        [[hash]] ->
          if checkHash password hash
            then f db
            else return err
        _ -> return err

doLogin c user password =
  verifyPassword c user password Nothing $ \db -> do
              apiKey <- newApiKey
              CTime time <- utSeconds <$> getUnixTime
              execute
                db
                "INSERT INTO apikeys (apikey,user,created) VALUES (?, ?, ?)"
                (apiKey, user, time)
              return $ Just apiKey

doCheckPassword c user password =
  verifyPassword c user password False (const $ pure True)

doVerify c apikey =
  dbBracket c $ \db -> do
    res <-
      query
        db
        "SELECT users.user FROM apikeys JOIN users ON apikeys.user=users.user WHERE apikey = ?"
        (Only (apikey :: String)) :: IO [[String]]
    case res of
      [[user]] -> return $ Just user
      _ -> return Nothing

doLogout c apikey =
  dbBracket c $ \db ->
    execute db "DELETE FROM apikeys WHERE apikey = ?" (Only (apikey :: String))

doLogoutAll c user =
  dbBracket c $ \db ->
    execute db "DELETE FROM apikeys WHERE user = ?" (Only (user :: String))

createUser c user password =
  dbBracket c $ \db -> do
    pwhash <- newHash password
    execute
      db
      "INSERT INTO users (user, pwhash) VALUES (?,?)"
      (user :: String, pwhash :: String)

deleteUser c user =
  dbBracket c $ \db ->
    withTransaction db $ do
      execute db "DELETE FROM users WHERE user = ?" (Only (user :: String))
      execute db "DELETE FROM apikeys WHERE user = ?" (Only (user :: String))

setPassword c user password =
  dbBracket c $ \db -> do
    pwhash <- newHash password
    execute
      db
      "UPDATE users SET pwhash = ? WHERE user = ?"
      (pwhash :: String, user :: String)
