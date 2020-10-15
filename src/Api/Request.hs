{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Request where

import Api.Common (notFound, success)
import Control.Monad (forM, forM_, join)
import Data.Aeson (KeyValue ((.=)), object)
import Data.BareProperty (BareProperty)
import qualified Data.BareProperty as Bare
import Data.Environment (EnvAction, jsonData, param, status)
import Data.List ((\\))
import qualified Data.Model.Property as P
import qualified Data.Model.PropertyType as PropertyType
import qualified Data.Model.Request as R (Request (_id))
import Data.PropertyWithoutId (PropertyWithoutId)
import qualified Data.PropertyWithoutId as PWI
import qualified Data.ReqWithProps as RWP
import qualified Data.ReqWithPropsWithoutId as RWPWI
import qualified Database.Common as Db
import Database.Selda
import Database.Selda.PostgreSQL (PG)
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
    _ -> notFound

getDetails :: EnvAction ()
getDetails = getSubsetOfProps $ \prop ->
  prop ! #propertyType .== literal PropertyType.Detail
    .|| prop ! #propertyType .== literal PropertyType.File

getComments :: EnvAction ()
getComments = getSubsetOfProps $ \prop ->
  prop ! #propertyType .== literal PropertyType.Result
    .|| prop ! #propertyType .== literal PropertyType.ResultFile

getResults :: EnvAction ()
getResults = getSubsetOfProps $ \prop -> prop ! #propertyType .== literal PropertyType.Result

getSubsetOfProps :: (Row (Inner PG) P.Property -> Col (Inner PG) Bool) -> EnvAction ()
getSubsetOfProps keep = do
  reqId <- param "_id"
  props <-
    query $ select Table.properties `suchThat` (\prop -> fromReq reqId prop .&& keep prop)
  success props
  where
    fromReq :: ID R.Request -> Row t P.Property -> Col t Bool
    fromReq reqId prop = prop ! #requestId .== literal reqId

-- TODO Add transactions
updateWithProps :: EnvAction ()
updateWithProps = do
  RWP.RWP {RWP.req, RWP.props} <- jsonData
  let reqId = R._id req

  repeatedProps <-
    fmap join . forM props $ \prop ->
      query $ do
        p <- select Table.properties
        restrict $
          p ! #requestId .== literal reqId
            .&& p ! #propertyName .== literal (PWI.propertyName prop)
            .&& p ! #propertyType .== literal (PWI.propertyType prop)
        return p

  let ignoredProps = filter (memberBy propEq props) repeatedProps
  let updatedProps = repeatedProps \\ ignoredProps

  -- Deactivate old properties
  forM_ updatedProps $ \prop ->
    update_
      Table.properties
      (\p -> p ! #_id .== literal (P._id prop))
      (\p -> p `with` [#active := false])

  -- Insert new properties
  insert_ Table.properties $
    fmap (addId def) $
      filter (not . memberBy (flip propEq) ignoredProps) props

  -- Update the request
  Db.update Table.requests reqId req
  where
    memberBy eq xs x = any (eq x) xs
    propEqName p q =
      (P.propertyName p == PWI.propertyName q) && (P.propertyType p == PWI.propertyType q)
    propEq p q =
      propEqName p q && (P.propertyData p == PWI.propertyData q)

createWithProps :: EnvAction ()
createWithProps = do
  RWPWI.RWP {RWPWI.req, RWPWI.props} <- jsonData
  newRequest <- Db.create Table.requests req
  insert_ Table.properties (addId def . addReqId (R._id newRequest) <$> props)
  success newRequest
  status created201

addReqId :: ID R.Request -> BareProperty -> PropertyWithoutId
addReqId reqId Bare.Property {..} =
  PWI.Property reqId authorId propertyType propertyName propertyData dateAdded active
