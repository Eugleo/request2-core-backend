{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Request where

import Api.Common (failure, success)
import Control.Monad (forM_)
import Data.Aeson hiding (json)
import Data.BareProperty (BareProperty)
import qualified Data.BareProperty as Bare
import Data.Environment
import Data.Model.Request (Request (_id))
import Data.PropertyWithoutId (PropertyWithoutId)
import qualified Data.PropertyWithoutId as PWI
import qualified Data.ReqWithProps as RWP
import qualified Data.ReqWithPropsWithoutId as RWPWI
import qualified Database.Common as Db
import Database.Selda
import qualified Database.Table as Table
import Network.HTTP.Types.Status (created201)
import Utils.Id.AddId (addId)

getWithProps :: EnvAction ()
getWithProps = do
  reqId <- param "_id"
  res <- query $ select Table.requests `suchThat` (\req -> req ! #_id .== literal reqId)
  case res of
    [req] -> do
      props <-
        query $
          select Table.properties `suchThat` (\prop -> prop ! #requestId .== literal reqId)
      success $ object ["request" .= req, "properties" .= props]
    _ -> failure

updateWithProps :: EnvAction ()
updateWithProps = do
  RWP.RWP {RWP.req, RWP.props} <- jsonData
  let reqId = _id req
  -- Deactivate the old props that are beng overwritten
  forM_ (PWI.propertyPath <$> props) $ \path ->
    update_
      Table.properties
      (\p -> p ! #requestId .== literal reqId .&& p ! #propertyPath .== literal path)
      (\p -> p `with` [#active := false])
  -- Add the new props
  insert_ Table.properties (addId def <$> props)
  -- Update the request
  Db.update Table.requests reqId req

createWithProps :: EnvAction ()
createWithProps = do
  RWPWI.RWP {RWPWI.req, RWPWI.props} <- jsonData
  newRequest <- Db.create Table.requests req
  insert_ Table.properties (addId def . addReqId (_id newRequest) <$> props)
  success newRequest
  status created201

addReqId :: ID Request -> BareProperty -> PropertyWithoutId
addReqId reqId Bare.Property {..} =
  PWI.Property reqId authorId propertyPath propertyData dateAdded active
