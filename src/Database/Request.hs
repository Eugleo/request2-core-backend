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
  envIO $
    execute
      db
      "INSERT INTO Requests (author_id, team_id, status, type, date_added) \
      \ VALUES (?, ?, ?, ?, ?)"
      req
  rowID <- envIO $ lastInsertRowId db
  return $ WithID (fromIntegral rowID) req

get :: ID -> EnvAction (Maybe (WithID Request))
get reqID = do
  db <- askDB
  res <- envIO $ query db "SELECT * FROM Requests WHERE request_id = ?" (Only reqID)
  case res of
    [req] -> return . Just $ req
    _ -> return Nothing

getAll :: EnvAction [WithID Request]
getAll = askDB >>= \db -> envIO $ query_ db "SELECT * FROM Requests"

getWithProperties :: ID -> EnvAction (Maybe (WithID Request, [WithID Property]))
getWithProperties reqID = do
  db <- askDB
  res <- envIO $ query db "SELECT * FROM Requests WHERE request_id = ?" (Only reqID)
  case res of
    [req] -> do
      props <- envIO $ query db "SELECT * FROM Properties WHERE request_id = ?" (Only reqID)
      return $ Just (req, props)
    _ -> return Nothing

addProperty :: ID -> Property -> EnvAction (WithID Property)
addProperty reqID prop@P.Property {..} = do
  db <- askDB
  envIO $
    execute
      db
      "INSERT INTO Properties (request_id, author_id, name, data, date_added, deleted) \
      \ VALUES (?, ?, ?, ?, ?, ?)"
      (reqID, authorID, propertyType, propertyData, dateAdded, deleted)
  rowID <- envIO $ lastInsertRowId db
  return $ WithID (fromIntegral rowID) prop

updateRequest :: (WithID Request, [Property]) -> EnvAction ()
updateRequest (WithID reqID R.Request {..}, props) = do
  db <- askDB
  -- Remove old properties
  envIO $
    execute
      db
      "UPDATE Properties \
      \ SET deleted = ? \
      \ WHERE request_id = ?"
      (Only reqID)
  -- Add new properties
  mapM_ (addProperty reqID) props
  -- Update the request itself
  envIO $
    execute
      db
      "UPDATE Requests \
      \ SET author_id = ?, team_id = ?, status = ?, type = ?, date_added = ? \
      \ WHERE request_id = ?"
      (authorID, teamID, status, requestType, created)

removeProperty :: ID -> EnvAction ()
removeProperty propID = do
  db <- askDB
  envIO $
    execute
      db
      "UPDATE Properties \
      \ SET deleted = ? \
      \ WHERE property_id = ?"
      (True, propID)
