{-# LANGUAGE OverloadedStrings #-}

module API.Announcement where

import Config
import qualified Database.Announcement as DB
import Environment
import Network.HTTP.Types.Status --TODO remove
import Web.Scotty
import WithID

{-
create :: ServerConfig -> ActionM ()
create c = do
  ann <- jsonData
  new <- liftAndCatchIO $ DB.add c ann
  json new
  status created201

-- TODO Only authors + admins should be able to edit
-- ASK atm the put request sends a whole Announcement object (which then replaces the current one)
-- shouldn't it maybe just send the requested changes as form fields?
edit :: ServerConfig -> ActionM ()
edit c = do
  annID <- param "ann_id"
  ann <- jsonData
  liftAndCatchIO $ DB.edit c $ WithID annID ann
  status ok200

-- TODO Only authors + admins should be able to deactivate
deactivate :: ServerConfig -> ActionM ()
deactivate c = do
  annID <- param "ann_id"
  liftAndCatchIO $ DB.deactivate c annID
  status ok200
-}

get :: EnvAction ()
get = do
  annID <- lift $ param "ann_id"
  c <- askConfig --TODO migrate
  ann <- envIO $ DB.get c annID
  case ann of
    Just a -> lift $ json a >> status ok200
    Nothing -> lift $ status notFound404 >> finish

getAll :: EnvAction ()
getAll = askConfig >>= envIO . DB.getAll >>= \x -> lift $json x
