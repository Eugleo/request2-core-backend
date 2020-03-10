{-# LANGUAGE OverloadedStrings #-}

module API.Announcement where

import Config
import qualified Database.Announcement as DB
import Network.HTTP.Types.Status
import Web.Scotty
import WithID

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

get :: ServerConfig -> ActionM ()
get c = do
  annID <- param "ann_id"
  ann <- liftAndCatchIO $ DB.get c annID
  case ann of
    Just a -> json a >> status ok200
    Nothing -> status notFound404 >> finish

getAll :: ServerConfig -> ActionM ()
getAll c = liftAndCatchIO (json <$> DB.getAll c) >> status ok200
