{-# LANGUAGE RecordWildCards #-}

module Database.Team where

-- TODO Support search & filter
import Data.Functor (void)
import Database.PostgreSQL.Simple
import Environment
import Model.Team
import WithID (ID, WithID (..))

add :: Team -> EnvAction (WithID Team)
add team = do
  db <- askDB
  [Only rowID] <-
    envIO $
      query
        db
        "INSERT INTO teams (name, active) \
        \ VALUES (?, ?) RETURNING team_id"
        team
  return $ WithID rowID team

deactivate :: ID -> EnvAction ()
deactivate teamID = do
  db <- askDB
  void . envIO $
    execute db "UPDATE teams SET active = ? WHERE team_id = ?" (False, teamID)

get :: ID -> EnvAction (Maybe (WithID Team))
get teamID = do
  db <- askDB
  res <-
    envIO $
      query
        db
        "SELECT team_id, name, active FROM teams WHERE team_id = ?"
        (Only teamID)
  case res of
    [team] -> return . Just $ team
    _ -> return Nothing

-- TODO Support order by date created (i.e. ID) or by name
getMany :: Integer -> Integer -> EnvAction [WithID Team]
getMany lim offset = do
  db <- askDB
  let limit = min lim 500
  envIO $
    query
      db
      "SELECT team_id, name, active FROM teams LIMIT ? OFFSET ?"
      (limit, offset)

edit :: WithID Team -> EnvAction ()
edit (WithID teamID Team {..}) = do
  db <- askDB
  void . envIO $
    execute
      db
      "UPDATE teams SET name = ?, active = ? WHERE team_id = ?"
      (name, active, teamID)

count :: EnvAction Integer
count = do
  db <- askDB
  [Only len] <- envIO $ query_ db "SELECT COUNT(team_id) FROM teams"
  return len
