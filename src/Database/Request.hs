{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Request where

import Database.SQLite.Simple
import Environment (EnvAction, askDB, envIO)
import Model.Property (Property)
import qualified Model.Property as P
import Model.Request (Request)
import qualified Model.Request as R
import WithID

add :: Request -> EnvAction (WithID Request)
add req = do
  db <- askDB
  rowID <-
    envIO $ do
      execute
        db
        "INSERT INTO requests (user_id, team_id, status, type, created) \
        \ VALUES (?, ?, ?, ?, ?)"
        req
      fromIntegral <$> lastInsertRowId db
  return $ WithID rowID req

get :: ID -> EnvAction (Maybe (WithID Request))
get reqID = do
  db <- askDB
  res <-
    envIO $ query db "SELECT * FROM requests WHERE request_id = ?" (Only reqID)
  case res of
    [req] -> return . Just $ req
    _ -> return Nothing

getAll :: EnvAction [WithID Request]
getAll = askDB >>= \db -> envIO $ query_ db "SELECT * FROM requests"

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

addProperty :: ID -> Property -> EnvAction (WithID Property)
addProperty reqID prop@P.Property {..} = do
  db <- askDB
  rowID <- envIO $ do
    execute
      db
      "INSERT INTO properties (request_id, user_id, type, data, created, enabled) \
      \ VALUES (?, ?, ?, ?, ?, ?)"
      (reqID, authorID, propertyType, propertyData, dateAdded, enabled)
    fromIntegral <$> lastInsertRowId db
  return $ WithID rowID prop

updateRequest :: (WithID Request, [Property]) -> EnvAction ()
updateRequest (WithID reqID R.Request {..}, props) = do
  db <- askDB
  -- Remove old properties
  envIO $
    execute
      db
      "UPDATE properties \
      \ SET enabled = ? \
      \ WHERE request_id = ?"
      (Only reqID)
  -- Add new properties
  mapM_ (addProperty reqID) props
  -- Update the request itself
  envIO $
    execute
      db
      "UPDATE requests \
      \ SET user_id = ?, team_id = ?, status = ?, type = ?, created = ? \
      \ WHERE request_id = ?"
      (authorID, teamID, status, requestType, created)

removeProperty :: ID -> EnvAction ()
removeProperty propID = do
  db <- askDB
  envIO $
    execute
      db
      "UPDATE properties \
      \ SET enabled = ? \
      \ WHERE property_id = ?"
      (True, propID)
