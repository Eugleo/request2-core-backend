{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Common where

import AddId
import Data.Aeson ((.=), FromJSON, ToJSON, object, toJSON)
import Data.Maybe (listToMaybe)
import Database.Selda hiding (update)
import qualified Database.Selda as Selda (update)
import Environment
import Network.HTTP.Types.Status (notFound404, ok200)

createFrom ::
  forall a b w.
  (AddId a b w, Relational b, ToJSON b) =>
  Table b ->
  a ->
  EnvAction ()
createFrom tbl val = do
  let valWithDef = addId def val :: b
  rowId <- insertWithPK tbl [valWithDef]
  let valWithId = addId rowId val :: b
  json valWithId

create ::
  forall a b w.
  (AddId a b w, Relational b, FromJSON a, ToJSON b) =>
  Table b ->
  EnvAction ()
create tbl = do
  val <- jsonData :: EnvAction a
  createFrom tbl val

getBy ::
  (Relational b, HasField "_id" b, FieldType "_id" b ~ ID b) =>
  Table b ->
  ID b ->
  EnvAction (Maybe b)
getBy tbl valId = do
  res <- query $ do
    val <- select tbl
    restrict (val ! #_id .== literal valId)
    return val
  return (listToMaybe res)

get ::
  (Relational b, ToJSON b, HasField "_id" b, FieldType "_id" b ~ ID b) =>
  Table b ->
  EnvAction ()
get tbl = do
  valId <- param "_id" :: EnvAction (ID b)
  res <- query $ do
    val <- select tbl
    restrict (val ! #_id .== literal valId)
    return val
  maybe (status notFound404 >> finish) json (listToMaybe res)

getMany ::
  (Relational b, ToJSON b, HasField "_id" b, FieldType "_id" b ~ ID b) =>
  Table b ->
  EnvAction ()
getMany tbl = do
  lim <- param "limit"
  offset <- param "offset"
  vals <- query $ limit offset lim (select tbl)
  res <- query . aggregate $ do
    v <- select tbl
    return (count (v ! #_id))
  maybe
    (status notFound404 >> finish)
    (\total -> json (object ["anns" .= toJSON vals, "total" .= total]))
    (listToMaybe res)

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

delete ::
  ( Relational b,
    HasField "_id" b,
    FieldType "_id" b ~ ID b
  ) =>
  Table b ->
  EnvAction ()
delete tbl = do
  valId <- param "_id" :: EnvAction (ID b)
  n <- deleteFrom tbl (\val -> val ! #_id .== literal valId)
  json $ object ["changed" .= n]
  status ok200

deactivate ::
  ( Relational b,
    HasField "_id" b,
    FieldType "_id" b ~ ID b,
    HasField "active" b,
    FieldType "active" b ~ Bool
  ) =>
  Table b ->
  EnvAction ()
deactivate tbl = do
  valId <- param "_id" :: EnvAction (ID b)
  n <-
    Selda.update
      tbl
      (\val -> val ! #_id .== literal valId)
      (\val -> val `with` [#active := false])
  json $ object ["changed" .= n]
  status ok200
