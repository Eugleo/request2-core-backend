{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Common where

import Data.Environment
import Data.Maybe (listToMaybe)
import Database.Selda hiding (update)
import qualified Database.Selda as Selda
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
  res <- query $ select tbl `suchThat` (\val -> val ! #_id .== literal valId)
  return (listToMaybe res)

update ::
  ( Relational b,
    HasField "_id" b,
    FieldType "_id" b ~ ID b
  ) =>
  Table b ->
  ID b ->
  b ->
  EnvAction ()
update tbl valId val =
  Selda.update_
    tbl
    (\v -> v ! #_id .== literal valId)
    (const (row val))
