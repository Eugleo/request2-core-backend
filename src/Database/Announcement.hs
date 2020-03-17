{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Announcement where

import Database.SQLite.Simple
import Environment (EnvAction, askDB, envIO)
import Model.Announcement (Announcement (..))
import WithID

-- ASK is lastInsertRowId always equal to the ann_id of the added announcement?
add :: Announcement -> EnvAction (WithID Announcement)
add ann = do
  db <- askDB
  envIO $
    execute
      db
      "INSERT INTO announcements (title, body, user_id, created, active) \
      \ VALUES (?, ?, ?, ?, ?)"
      ann
  rowID <- envIO $ lastInsertRowId db
  return $ WithID (fromIntegral rowID) ann

-- ASK Should we instead allow the editing of title+body only?
edit :: WithID Announcement -> EnvAction ()
edit (WithID annID Ann {..}) = do
  db <- askDB
  envIO $
    execute
      db
      "UPDATE announcements \
      \ SET title = ?, body = ?, user_id = ?, created = ?, active = ? \
      \ WHERE announcement_id = ?"
      (title, body, authorID, created, active, annID)

deactivate :: ID -> EnvAction ()
deactivate annID = do
  db <- askDB
  envIO $
    execute db "UPDATE announcements SET active = ? WHERE announcement_id = ?" (False, annID)

get :: ID -> EnvAction (Maybe (WithID Announcement))
get annID = do
  db <- askDB
  res <- envIO $ query db "SELECT * FROM announcements WHERE announcement_id = ?" (Only annID)
  case res of
    [ann] -> return . Just $ ann
    _ -> return Nothing

getAll :: EnvAction [WithID Announcement]
getAll = do
  db <- askDB
  envIO $
    query_ db "SELECT announcement_id, title, body, user_id, created, active FROM announcements"