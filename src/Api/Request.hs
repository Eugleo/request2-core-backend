{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Request where

import Data.Environment
import Data.Model.Property (Property)
import Data.Model.Request (Request (..))
import qualified Data.ReqWithProps as RWP
import qualified Database.Common as Db
import Database.Selda
import qualified Database.Table as Table
import Utils.Id.AddId (addId)

getWithProps :: EnvAction (Maybe (Request, [Property]))
getWithProps = do
  reqId <- param "_id"
  res <- query $ select Table.requests `suchThat` (\req -> req ! #_id .== literal reqId)
  case res of
    [req] -> do
      props <-
        query $
          select Table.properties `suchThat` (\prop -> prop ! #requestId .== literal reqId)
      return $ Just (req, props)
    _ -> return Nothing

updateWithProps :: EnvAction ()
updateWithProps = do
  RWP.RWP {RWP.req, RWP.props} <- jsonData
  let reqId = _id req
  -- Deactivate old props
  update_
    Table.properties
    (\p -> p ! #requestId .== literal reqId)
    (\p -> p `with` [#active := false])
  -- Add the new props
  insert_ Table.properties (addId def <$> props)
  -- Update the request
  Db.update Table.requests reqId req
