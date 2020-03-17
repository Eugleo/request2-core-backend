{-# LANGUAGE OverloadedStrings #-}

module API.Announcement where

import qualified Database.Announcement as DB
import Environment
import Network.HTTP.Types.Status
import WithID

create :: EnvAction ()
create = do
  ann <- jsonData
  new <- DB.add ann
  json new
  status created201

-- TODO Only authors + admins should be able to edit
-- ASK atm the put request sends a whole Announcement object (which then replaces the current one)
-- shouldn't it maybe just send the requested changes as form fields?
edit :: EnvAction ()
edit = do
  annID <- param "ann_id"
  ann <- jsonData
  DB.edit $ WithID annID ann

-- TODO Only authors + admins should be able to deactivate
deactivate :: EnvAction ()
deactivate = do
  annID <- param "ann_id"
  DB.deactivate annID

get :: EnvAction ()
get = do
  annID <- param "ann_id"
  ann <- DB.get annID
  case ann of
    Just a -> json a
    Nothing -> status notFound404

getAll :: EnvAction ()
getAll = DB.getAll >>= json
