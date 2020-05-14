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
  res <- query $ do
    request <- select Table.requests
    restrict (request ! #_id .== literal reqId)
    return request
  case res of
    [req] -> do
      props <- query $ do
        prop <- select Table.properties
        restrict (prop ! #requestId .== literal reqId)
        return prop
      return $ Just (req, props)
    _ -> return Nothing

-- TODO Add request update

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
