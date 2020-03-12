{-# LANGUAGE OverloadedStrings #-}

-- TODO Add missing fields to every table
-- TODO Check if dates are integers or real
module Database.Schema
  ( createDatabase
  ) where

import Config
import Database.SQLite.Simple

createDatabase :: ServerConfig -> IO ()
createDatabase cfg =
  withConnection (dbPath cfg) $ \conn -> do
    let e = execute_ conn
    e "CREATE TABLE IF NOT EXISTS teams ( \
      \ team_id BIGINT PRIMARY KEY, \
      \ name TEXT UNIQUE, \
      \ active INTEGER)"
    -- search for active teams by name
    e "CREATE INDEX IF NOT EXISTS \
      \ team_active_name ON teams(active, name)"

    e "CREATE TABLE IF NOT EXISTS Users ( \
      \ user_id BIGINT PRIMARY KEY, \
      \ email TEXT UNIQUE, \
      \ name TEXT, \
      \ pw_hash TEXT, \
      \ team_id BIGINT REFERENCES Teams (team_id), \
      \ roles TEXT, \
      \ created BIGINT)"
    -- lsearch for user login
    e "CREATE INDEX IF NOT EXISTS \
      \ users_email on users(email)"
    -- listing the users in team
    e "CREATE INDEX IF NOT EXISTS \
      \ users_team_name ON users(team_id, name)"

    e "CREATE TABLE IF NOT EXISTS Requests ( \
      \ request_id BIGINT PRIMARY KEY, \
      \ user_id BIGINT REFERENCES Users (user_id), \
      \ team_id BIGINT REFERENCES Teams (team_id), \
      \ status TEXT, \
      \ type TEXT, \
      \ created BIGINT)"
    -- listing requests by properties
    e "CREATE INDEX IF NOT EXISTS \
      \ requests_author_created ON requests(user_id,status,created)"
    e "CREATE INDEX IF NOT EXISTS \
      \ requests_team_created ON requests(team_id,status,created)"
    e "CREATE INDEX IF NOT EXISTS \
      \ requests_status_type_created ON requests(status, type, created)"

    e "CREATE TABLE IF NOT EXISTS Announcements ( \
      \ announcement_id BIGINT PRIMARY KEY, \
      \ title TEXT, \
      \ body TEXT, \
      \ author_id BIGINT REFERENCES Users (user_id), \
      \ created BIGINT, \
      \ active INTEGER)"
    -- primary display announcement listing
    e "CREATE INDEX IF NOT EXISTS \
      \ announcements_active ON announcements(active, created)"

    e "CREATE TABLE IF NOT EXISTS Properties ( \
      \ property_id BIGINT PRIMARY KEY, \
      \ request_id BIGINT REFERENCES Requests (request_id), \
      \ author_id BIGINT REFERENCES Users (user_id), \
      \ type TEXT, \
      \ data TEXT, \
      \ created BIGINT, \
      \ enabled INTEGER)"
    -- for displaying the request single-page
    e "CREATE INDEX IF NOT EXISTS \
      \ properties_on_request ON properties(enabled, request_id, type, created)"

    e "CREATE TABLE IF NOT EXISTS ApiKeys ( \
      \ api_key TEXT PRIMARY KEY, \
      \ user_id BIGINT REFERENCES Users (user_id), \
      \ created BIGINT)"
    -- search for api_key on user verification
    e "CREATE INDEX IF NOT EXISTS \
      \ apikeys_userkey ON apikeys(user_id, api_key)"
    -- apikey deprecation
    e "CREATE INDEX IF NOT EXISTS \
      \ apikeys_date ON apikeys(created)"
