{-# LANGUAGE OverloadedStrings #-}

-- TODO Add missing fields to every table
module Database.Schema
  ( createDatabase,
  )
where

import Config
import Database.SQLite.Simple

createDatabase :: ServerConfig -> IO ()
createDatabase cfg =
  withConnection (dbPathStr cfg) $ \conn -> do
    let e = execute_ conn
    e
      "CREATE TABLE IF NOT EXISTS teams ( \
      \ team_id INTEGER PRIMARY KEY, \
      \ name TEXT UNIQUE, \
      \ active INTEGER)"
    -- search for active teams by name
    e
      "CREATE INDEX IF NOT EXISTS \
      \ team_active_name ON teams(active, name)"
    e
      "CREATE TABLE IF NOT EXISTS users ( \
      \ user_id INTEGER PRIMARY KEY, \
      \ email TEXT UNIQUE, \
      \ name TEXT, \
      \ pw_hash TEXT, \
      \ team_id INTEGER REFERENCES teams (team_id), \
      \ roles TEXT, \
      \ created INTEGER)"
    -- lsearch for user login
    e
      "CREATE INDEX IF NOT EXISTS \
      \ users_email on users(email)"
    -- listing the users in team
    e
      "CREATE INDEX IF NOT EXISTS \
      \ users_team_name ON users(team_id, name)"
    e
      "CREATE TABLE IF NOT EXISTS requests ( \
      \ request_id INTEGER PRIMARY KEY, \
      \ user_id INTEGER REFERENCES users (user_id), \
      \ team_id INTEGER REFERENCES teams (team_id), \
      \ status TEXT, \
      \ type TEXT, \
      \ created INTEGER)"
    -- listing requests by properties
    e
      "CREATE INDEX IF NOT EXISTS \
      \ requests_author_created ON requests(user_id,status,created)"
    e
      "CREATE INDEX IF NOT EXISTS \
      \ requests_team_created ON requests(team_id,status,created)"
    e
      "CREATE INDEX IF NOT EXISTS \
      \ requests_status_type_created ON requests(status, type, created)"
    e
      "CREATE TABLE IF NOT EXISTS announcements ( \
      \ announcement_id INTEGER PRIMARY KEY, \
      \ title TEXT, \
      \ body TEXT, \
      \ user_id INTEGER REFERENCES users (user_id), \
      \ created INTEGER, \
      \ active INTEGER)"
    -- primary display announcement listing
    e
      "CREATE INDEX IF NOT EXISTS \
      \ announcements_active ON announcements(active, created)"
    e
      "CREATE TABLE IF NOT EXISTS properties ( \
      \ property_id INTEGER PRIMARY KEY, \
      \ request_id INTEGER REFERENCES requests (request_id), \
      \ user_id INTEGER REFERENCES users (user_id), \
      \ type TEXT, \
      \ data TEXT, \
      \ created INTEGER, \
      \ enabled INTEGER)"
    -- for displaying the request single-page
    e
      "CREATE INDEX IF NOT EXISTS \
      \ properties_on_request ON properties(enabled, request_id, type, created)"
    e
      "CREATE TABLE IF NOT EXISTS api_keys ( \
      \ api_key TEXT PRIMARY KEY, \
      \ user_id INTEGER REFERENCES users (user_id), \
      \ created INTEGER)"
    -- search keys for full logout
    e
      "CREATE INDEX IF NOT EXISTS \
      \ api_keys_userkey ON api_keys(user_id, api_key)"
    -- apikey deprecation
    e
      "CREATE INDEX IF NOT EXISTS \
      \ api_keys_date ON api_keys(created)"
