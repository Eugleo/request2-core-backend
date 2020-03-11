{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- TODO Add missing fields to every table
-- TODO Check if dates are integers or real

module Database.General
  ( createDatabase,
    withConfig,
    ID,
  )
where

import Config
import Database.SQLite.Simple
import Text.RawString.QQ (r)

type ID = Integer

createTeamsTable :: Query
createTeamsTable =
  [r|
    CREATE TABLE IF NOT EXISTS Teams (
      team_id INTEGER PRIMARY KEY,
      name TEXT UNIQUE,
      active INTEGER
    );
  |]

createUsersTable :: Query
createUsersTable =
  [r|
    CREATE TABLE IF NOT EXISTS Users (
      user_id INTEGER PRIMARY KEY,
      email TEXT UNIQUE,
      name TEXT,
      pw_hash TEXT,
      team_id INTEGER REFERENCES Teams (team_id),
      roles TEXT
    );
  |]

createRequestsTable :: Query
createRequestsTable =
  [r|
    CREATE TABLE IF NOT EXISTS Requests (
      request_id INTEGER PRIMARY KEY,
      author_id INTEGER REFERENCES Users (user_id),
      team_id INTEGER REFERENCES Teams (team_id),
      type TEXT,
      date_added INTEGER
    );
  |]

createAnnouncementsTable :: Query
createAnnouncementsTable =
  [r|
    CREATE TABLE IF NOT EXISTS Announcements (
      announcement_id INTEGER PRIMARY KEY,
      title TEXT,
      body TEXT,
      author_id INTEGER REFERENCES Users (user_id),
      date_created INTEGER,
      date_modified INTEGER,
      active INTEGER
    );
  |]

createPropertiesTable :: Query
createPropertiesTable =
  [r|
    CREATE TABLE IF NOT EXISTS Properties (
      property_id INTEGER PRIMARY KEY,
      request_id INTEGER REFERENCES Requests (request_id),
      author_id INTEGER REFERENCES Users (user_id),
      name TEXT,
      data TEXT,
      date_added INTEGER,
      deleted INTEGER
    );
  |]

createApiKeysTable :: Query
createApiKeysTable =
  [r|
    CREATE TABLE IF NOT EXISTS ApiKeys (
      api_key TEXT PRIMARY KEY,
      user_id INTEGER REFERENCES Users (user_id),
      date_created INTEGER
    );
  |]

withConfig :: ServerConfig -> (Connection -> IO a) -> IO a
withConfig cfg = withConnection (dbPath cfg)

createDatabase :: ServerConfig -> IO ()
createDatabase cfg =
  mapM_
    (\q -> withConfig cfg (`execute_` q))
    [ createTeamsTable,
      createUsersTable,
      createRequestsTable,
      createAnnouncementsTable,
      createPropertiesTable,
      createApiKeysTable
    ]
