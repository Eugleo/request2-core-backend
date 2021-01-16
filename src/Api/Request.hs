{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Request where

import Api.Common (notFound, success)
import Control.Monad (forM, forM_, join)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Lens (key, _Object)
import Data.Aeson.Types (Object, Value)
import Data.Environment (EnvAction, askUserInfo, envIO, jsonData, jsonParam, param, runParser, status)
import Data.List ((\\))
import Data.Model.DateTime (now)
import qualified Data.Model.Property as P
import qualified Data.Model.Request as R (Request (..), parseRequest, parseRequestId)
import Data.Model.Status (Status (Pending))
import Data.Model.Team (Team)
import qualified Data.UserInfo as UI
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


getComments :: EnvAction ()
getComments = do
    reqId <- param "_id"
    comments <- query $ select Table.comments `suchThat` \c -> c ! #requestId .== literal reqId
    success comments


-- TODO Add transactions
updateWithProps :: EnvAction ()
updateWithProps = do
    ((title, teamId, _), properties) <- getRequestAndProps
    reqId <- runParser R.parseRequestId =<< jsonParam "request" (key "request")

    repeatedProps <-
        fmap join . forM properties $ \(name, value) ->
            query $
                select Table.properties `suchThat` \p ->
                    p ! #requestId .== literal reqId .&& p ! #name .== literal name

    let ignoredProps = filter (memberBy propEq properties) repeatedProps
    let updatedProps = repeatedProps \\ ignoredProps

    -- Deactivate old properties
    forM_ updatedProps $ \prop ->
        update_
            Table.properties
            ( \p ->
                p ! #requestId .== literal (P.requestId prop)
                    .&& p ! #name .== literal (P.name prop)
            )
            (\p -> p `with` [#active := false])

    -- Insert new properties
    dt <- envIO now
    userId <- UI.userId <$> askUserInfo
    insert_ Table.properties $
        (\(name, value) -> P.Property reqId userId name value dt True True)
            <$> filter (not . memberBy (flip propEq) ignoredProps) properties

    -- Update the request
    update_ Table.requests (\r -> r ! #_id .== literal reqId) $
        \r -> r `with` [#title := literal title, #teamId := literal teamId]
  where
    propEq p (name, value) = P.name p == name && (P.value p == value)
    memberBy eq xs x = any (eq x) xs


getRequestAndProps :: EnvAction ((Text, ID Team, Text), [(Text, Text)])
getRequestAndProps = do
    reqValue <- jsonParam "request" (key "request")
    propsValue <- jsonParam "properties" (key "properties")
    request <- runParser R.parseRequest reqValue
    properties <- runParser P.parseProperties propsValue
    return (request, properties)


createWithProps :: EnvAction ()
createWithProps = do
    ((title, teamId, requestType), properties) <- getRequestAndProps
    dt <- envIO now
    userId <- UI.userId <$> askUserInfo
    let newReq = R.Request def title userId teamId Pending requestType dt
    reqId <- insertWithPK Table.requests [newReq]
    insert_ Table.properties $
        fmap (\(name, value) -> P.Property reqId userId name value dt False True) properties
    success $ newReq{R._id = reqId}
    status created201
