{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Request where

import Data.Functor (void)
import Database.PostgreSQL.Simple
import Environment (EnvAction, askDB, envIO)
import Model.Property (Property)
import qualified Model.Property as P
import Model.Request (Request)
import qualified Model.Request as R

getWithproperties :: ID -> EnvAction (Maybe (WithID Request, [WithID Property]))
getWithproperties reqID = do
  db <- askDB
  res <-
    envIO $ query db "SELECT * FROM requests WHERE request_id = ?" (Only reqID)
  case res of
    [req] -> do
      props <-
        envIO $
          query db "SELECT * FROM properties WHERE request_id = ?" (Only reqID)
      return $ Just (req, props)
    _ -> return Nothing

updateRequest :: (WithID Request, [Property]) -> EnvAction ()
updateRequest (WithID reqID R.Request {..}, props) = do
  db <- askDB
  -- Remove old properties
  void . envIO $
    execute
      db
      "UPDATE properties \
      \ SET enabled = ? \
      \ WHERE request_id = ?"
      (Only reqID)
  -- Add new properties
  mapM_ (addProperty reqID) props
  -- Update the request itself
  void . envIO $
    execute
      db
      "UPDATE requests \
      \ SET user_id = ?, team_id = ?, status = ?, type = ?, created = ? \
      \ WHERE request_id = ?"
      (authorID, teamID, status, requestType, created)

removeProperty :: ID -> EnvAction ()
removeProperty propID = do
  db <- askDB
  void . envIO $
    execute
      db
      "UPDATE properties \
      \ SET enabled = ? \
      \ WHERE property_id = ?"
      (True, propID)
