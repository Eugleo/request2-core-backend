{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Request where

-- TODO Add request deactivation?

import Config
import Database.General (withConfig)
import Database.SQLite.Simple
import Model.Property (Property)
import qualified Model.Property as P
import Model.Request (Request)
import qualified Model.Request as R
import Text.RawString.QQ (r)
import WithID

add :: ServerConfig -> Request -> IO (WithID Request)
add c req = withConfig c $ \db -> do
  execute
    db
    [r|
    INSERT INTO Requests (author_id, team_id, status, type, date_added)
    VALUES (?, ?, ?, ?, ?)
    |]
    req
  rowID <- lastInsertRowId db
  return $ WithID (fromIntegral rowID) req

get :: ServerConfig -> ID -> IO (Maybe (WithID Request))
get c reqID = withConfig c $ \db -> do
  res <- query db "SELECT * FROM Requests WHERE request_id = ?" (Only reqID)
  case res of
    [req] -> return . Just $ req
    _ -> return Nothing

getAll :: ServerConfig -> IO [WithID Request]
getAll c = withConfig c (`query_` "SELECT * FROM Requests")

getWithProperties :: ServerConfig -> ID -> IO (Maybe (WithID Request, [WithID Property]))
getWithProperties c reqID = withConfig c $ \db -> do
  res <- query db "SELECT * FROM Requests WHERE request_id = ?" (Only reqID)
  case res of
    [req] -> do
      props <- query db "SELECT * FROM Properties WHERE request_id = ?" (Only reqID)
      return $ Just (req, props)
    _ -> return Nothing

addProperty :: ServerConfig -> ID -> Property -> IO (WithID Property)
addProperty c reqID prop@P.Property {..} = withConfig c $ \db -> do
  execute
    db
    [r|
    INSERT INTO Properties (request_id, author_id, name, data, date_added, deleted)
    VALUES (?, ?, ?, ?, ?, ?)
    |]
    (reqID, authorID, name, propertyData, dateAdded, deleted)
  rowID <- lastInsertRowId db
  return $ WithID (fromIntegral rowID) prop

updateRequest :: ServerConfig -> (WithID Request, [Property]) -> IO ()
updateRequest c (WithID reqID R.Request {..}, props) = withConfig c $ \db ->
  withTransaction db $ do
    -- Remove old properties
    execute
      db
      [r|
      UPDATE Properties
      SET deleted = ?
      WHERE request_id = ?
      |]
      (Only reqID)
    -- Add new properties
    mapM_ (addProperty c reqID) props
    -- Update the request itself
    execute
      db
      [r|
      UPDATE Requests
      SET author_id = ?, team_id = ?, status = ?, type = ?, date_added = ?
      WHERE request_id = ?
      |]
      (authorID, teamID, status, requestType, dateAdded)

removeProperty :: ServerConfig -> ID -> IO ()
removeProperty c propID = withConfig c $ \db ->
  withTransaction db $
    execute
      db
      [r|
      UPDATE Properties
      SET deleted = ?
      WHERE property_id = ?
      |]
      (True, propID)
