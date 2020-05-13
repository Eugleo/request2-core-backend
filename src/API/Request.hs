{-# LANGUAGE OverloadedLabels #-}

module Api.Request where

import Data.Functor (void)
import qualified Database.Schema as DB
import Database.Selda
import Environment (EnvAction, envIO, lift, param)
import Model.Property (Property)
import qualified Model.Property as P
import Model.Request (Request)
import qualified Model.Request as R

getWithproperties :: EnvAction (Maybe (Request, [Property]))
getWithproperties = do
  reqId <- param "_id"
  res <- lift . lift . query $ do
    request <- select DB.requests
    restrict (request ! #_id .== literal reqId)
    return request
  case res of
    [req] -> do
      props <- lift . lift . query $ do
        prop <- select DB.properties
        restrict (prop ! #requestId .== literal reqId)
        return prop
      return $ Just (req, props)
    _ -> return Nothing
-- TODO Add request update

-- updateRequest :: (WithID Request, [Property]) -> EnvAction ()
-- updateRequest (WithID reqID R.Request {..}, props) = do
--   db <- askDB
--   -- Remove old properties
--   void . envIO $
--     execute
--       db
--       "UPDATE properties \
--       \ SET enabled = ? \
--       \ WHERE request_id = ?"
--       (Only reqID)
--   -- Add new properties
--   mapM_ (addProperty reqID) props
--   -- Update the request itself
--   void . envIO $
--     execute
--       db
--       "UPDATE requests \
--       \ SET user_id = ?, team_id = ?, status = ?, type = ?, created = ? \
--       \ WHERE request_id = ?"
--       (authorID, teamID, status, requestType, created)
