{-# LANGUAGE OverloadedStrings #-}

-- TODO Add missing fields to every table
module Database.Schema
  ( createDatabase,
  )
where

import Config
import Data.Functor (void)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal (withConnection)

createDatabase :: ServerConfig -> IO ()
createDatabase cfg =
  withConnection (dbPathStr cfg) $ \conn -> do
    let e = execute_ conn
    e
      "CREATE TABLE IF NOT EXISTS teams ( \
      \ team_id INTEGER PRIMARY KEY, \
      \ name TEXT UNIQUE NOT NULL, \
      \ active INTEGER NOT NULL)"
    -- search for active teams by name
    e
      "CREATE INDEX IF NOT EXISTS \
      \ team_active_name ON teams(active, name)"
    e
      "CREATE TABLE IF NOT EXISTS users ( \
      \ user_id INTEGER PRIMARY KEY, \
      \ email TEXT UNIQUE NOT NULL, \
      \ name TEXT NOT NULL, \
      \ pw_hash TEXT NOT NULL, \
      \ team_id INTEGER NOT NULL REFERENCES teams (team_id), \
      \ roles TEXT NOT NULL, \
      \ created INTEGER NOT NULL)"
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
      \ user_id INTEGER NOT NULL REFERENCES users (user_id), \
      \ team_id INTEGER NOT NULL REFERENCES teams (team_id), \
      \ status TEXT NOT NULL, \
      \ type TEXT NOT NULL, \
      \ created INTEGER NOT NULL)"
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
      \ title TEXT NOT NULL, \
      \ body TEXT NOT NULL, \
      \ user_id INTEGER NOT NULL REFERENCES users (user_id), \
      \ created INTEGER NOT NULL, \
      \ active INTEGER NOT NULL)"
    -- primary display announcement listing
    e
      "CREATE INDEX IF NOT EXISTS \
      \ announcements_active ON announcements(active, created)"
    e
      "CREATE TABLE IF NOT EXISTS properties ( \
      \ property_id INTEGER PRIMARY KEY, \
      \ request_id INTEGER NOT NULL REFERENCES requests (request_id), \
      \ user_id INTEGER NOT NULL REFERENCES users (user_id), \
      \ type TEXT NOT NULL, \
      \ data TEXT NOT NULL, \
      \ created INTEGER NOT NULL, \
      \ enabled INTEGER NOT NULL)"
    -- for displaying the request single-page
    e
      "CREATE INDEX IF NOT EXISTS \
      \ properties_on_request ON properties(enabled, request_id, type, created)"
    e
      "CREATE TABLE IF NOT EXISTS api_keys ( \
      \ api_key TEXT PRIMARY KEY, \
      \ user_id INTEGER NOT NULL REFERENCES users (user_id), \
      \ created INTEGER NOT NULL)"
    -- search keys for full logout
    e
      "CREATE INDEX IF NOT EXISTS \
      \ api_keys_userkey ON api_keys(user_id, api_key)"
    -- apikey deprecation
    void $
      e
        "CREATE INDEX IF NOT EXISTS \
        \ api_keys_date ON api_keys(created)"
