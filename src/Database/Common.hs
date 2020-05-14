{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Common where

import Data.Aeson ((.=), FromJSON, ToJSON, object, toJSON)
import Data.Environment
import Data.Maybe (listToMaybe)
import Database.Selda hiding (update)
import qualified Database.Selda as Selda (update)
import Network.HTTP.Types.Status (notFound404, ok200)
import Utils.Id.AddId

create :: forall a b w. (AddId a b w, Relational b) => Table b -> a -> EnvAction b
create tbl val = do
  let valWithDef = addId def val :: b
  rowId <- insertWithPK tbl [valWithDef]
  let valWithId = addId rowId val :: b
  return valWithId

get ::
  (Relational b, HasField "_id" b, FieldType "_id" b ~ ID b) =>
  Table b ->
  ID b ->
  EnvAction (Maybe b)
get tbl valId = do
  res <- query $ do
    val <- select tbl
    restrict (val ! #_id .== literal valId)
    return val
  return (listToMaybe res)
-- TODO Add update

-- edit :: WithID Announcement -> EnvAction ()
-- edit (WithID annID Ann {..}) = do
--   db <- askDB
--   void . envIO $
--     execute
--       db
--       "UPDATE announcements \
--       \ SET title = ?, body = ?, user_id = ?, created = ?, active = ? \
--       \ WHERE announcement_id = ?"
--       (title, body, authorID, created, active, annID)

-- update ::
--   forall a b w.
--   ( AddId a b w,
--     Relational b,
--     FromJSON b,
--     ToJSON b,
--     HasField "_id" b,
--     FieldType "_id" b ~ ID b
--   ) =>
--   Table b ->
--   EnvAction ()

-- update tbl = do
--   valId <- param "_id" :: EnvAction (ID b)
--   val <- jsonData :: EnvAction b
--   n <-
--     lift . lift $
--       Selda.update
--         tbl
--         (\v -> v ! #_id .== literal valId)
--         (\v -> v `with` [_ . fromTup $ selectors val])
--   json $ object ["changed" .= n]
--   status ok200
