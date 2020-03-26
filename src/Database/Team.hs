module Database.Team where

import Database.SQLite.Simple
import Environment
import Model.Team
import WithID (ID, WithID)

get :: ID -> EnvAction (Maybe (WithID Team))
get teamID = do
  db <- askDB
  res <- envIO $ query db "SELECT * FROM teams WHERE team_id = ?" (Only teamID)
  case res of
    [team] -> return . Just $ team
    _ -> return Nothing
