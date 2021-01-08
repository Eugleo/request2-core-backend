{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Common where

import Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
import Data.Environment
import Data.Maybe (listToMaybe)
import qualified Database.Common as Db
import Database.Selda hiding (update)
import qualified Database.Selda as Selda (update)
import Network.HTTP.Types.Status (Status, created201, notFound404, ok200)
import Utils.Id.AddId


success :: ToJSON a => a -> EnvAction ()
success v = json (object ["data" .= toJSON v])


failure :: Text -> Status -> EnvAction a
failure msg st = do
    json $ object ["error" .= msg]
    status st
    finish


notFound :: EnvAction ()
notFound = failure "Error: Not found" notFound404


create ::
    forall a b w.
    (AddId a b w, Relational b, FromJSON a, ToJSON b) =>
    Table b ->
    EnvAction ()
create tbl = do
    val <- jsonData :: EnvAction a
    newVal <- Db.create tbl val
    json (object ["data" .= newVal])
    status created201


get ::
    ( Relational b,
      ToJSON b,
      HasField "_id" b,
      FieldType "_id" b ~ ID b
    ) =>
    Table b ->
    EnvAction ()
get tbl = do
    valId <- param "_id" :: EnvAction (ID b)
    val <- Db.get tbl valId
    maybe
        notFound
        success
        val


getMany ::
    ( Relational b,
      ToJSON b,
      HasField "_id" b,
      FieldType "_id" b ~ ID b
    ) =>
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
        notFound
        (\total -> success (object ["values" .= toJSON vals, "total" .= total]))
        (listToMaybe res)


update ::
    forall b.
    ( Relational b,
      FromJSON b,
      HasField "_id" b,
      FieldType "_id" b ~ ID b
    ) =>
    Table b ->
    EnvAction ()
update tbl = do
    valId <- param "_id" :: EnvAction (ID b)
    val <- jsonData :: EnvAction b
    Db.update tbl valId val


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
