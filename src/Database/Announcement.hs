{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Announcement where

import Config
import Database.General (withConfig)
import Database.SQLite.Simple
import Model.Announcement (Announcement (..))
import Text.RawString.QQ (r)
import WithID

-- ASK is lastInsertRowId always equal to the ann_id of the added announcement?
add :: ServerConfig -> Announcement -> IO (WithID Announcement)
add c ann = withConfig c $ \db -> do
  execute
    db
    [r|
    INSERT INTO Announcements (title, body, author_id, created, active)
    VALUES (?, ?, ?, ?, ?)
    |]
    ann
  rowID <- lastInsertRowId db
  return $ WithID (fromIntegral rowID) ann

-- ASK Should we instead allow the editing of title+body only?
edit :: ServerConfig -> WithID Announcement -> IO ()
edit c (WithID annID Ann {..}) = withConfig c $ \db ->
  withTransaction db $
    execute
      db
      [r|
      UPDATE Announcements
      SET title = ?, body = ?, author_id = ?, created = ?, active = ?
      WHERE ann_id = ?
      |]
      (title, body, authorID, created, active, annID)

deactivate :: ServerConfig -> ID -> IO ()
deactivate c annID = withConfig c $ \db ->
  withTransaction db $
    execute db "UPDATE Announcements SET active = ? WHERE ann_id = ?" (False, annID)

get :: ServerConfig -> ID -> IO (Maybe (WithID Announcement))
get c annID = withConfig c $ \db ->
  withTransaction db $ do
    res <- query db "SELECT * FROM Announcements WHERE ann_id = ?" (Only annID)
    case res of
      [ann] -> return . Just $ ann
      _ -> return Nothing

getAll :: ServerConfig -> IO [WithID Announcement]
getAll c = withConfig c $ \db ->
  withTransaction db $
    query_ db "SELECT * FROM Announcements"
