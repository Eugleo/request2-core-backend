{-# LANGUAGE RecordWildCards #-}

module Database.Team where

import Database.SQLite.Simple
import Environment
import Model.Team
import WithID (ID, WithID (..))

add :: Team -> EnvAction (WithID Team)
add team = do
  db <- askDB
  envIO $
    execute
      db
      "INSERT INTO teams (name, active) \
      \ VALUES (?, ?)"
      team
  rowID <- envIO $ lastInsertRowId db
  return $ WithID (fromIntegral rowID) team

deactivate :: ID -> EnvAction ()
deactivate teamID = do
  db <- askDB
  envIO $
    execute db "UPDATE teams SET active = ? WHERE team_id = ?" (False, teamID)

get :: ID -> EnvAction (Maybe (WithID Team))
get teamID = do
  db <- askDB
  res <- envIO $ query db "SELECT * FROM teams WHERE team_id = ?" (Only teamID)
  case res of
    [team] -> return . Just $ team
    _ -> return Nothing

-- TODO Support order by date created (i.e. ID) or by name
getMany :: Integer -> Integer -> EnvAction [WithID Team]
getMany lim offset = do
  db <- askDB
  let limit = min lim 500
  envIO $ query db "SELECT * FROM teams LIMIT ? OFFSET ?" (limit, offset)

edit :: WithID Team -> EnvAction ()
edit (WithID teamID Team {..}) = do
  db <- askDB
  envIO $
    execute
      db
      "UPDATE teams SET name = ?, active = ? WHERE team_id = ?"
      (name, active, teamID)
