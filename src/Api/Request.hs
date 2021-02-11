{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Request where

import Api.Common (notFound, success)
import Control.Monad (forM, forM_, join)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Lens (key, _Object)
import Data.Aeson.Types (Object, Parser, Value)
import Data.Environment (EnvAction, askUserInfo, envIO, jsonData, jsonParam, jsonParamText, param, runParser, status)
import Data.List (partition, (\\))
import qualified Data.Model.Comment as C
import Data.Model.DateTime (now)
import qualified Data.Model.Property as P
import Data.Model.Request (parseStatus)
import qualified Data.Model.Request as R (Request (..), parseRequestCreation, parseRequestEdit)
import Data.Model.Status (Status (Pending))
import Data.Model.Team (Team)
import Data.Text (unpack)
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


getProperties :: EnvAction ()
getProperties = do
    reqId <- param "_id"
    props <- query $ select Table.properties `suchThat` \c -> c ! #requestId .== literal reqId
    success props


-- TODO Add transactions
updateWithProps :: EnvAction ()
updateWithProps = do
    ((title, teamId), properties) <- getRequestAndProps R.parseRequestEdit
    reqId <- param "_id"

    allProps <-
        query $
            select Table.properties `suchThat` \p ->
                p ! #active .== literal True .&& p ! #requestId .== literal reqId

    let (repeatedProps, removedProps) = partition (memberBy (\p (name, _) -> P.name p == name) properties) allProps
    let (unchangedProps, updatedProps) = partition (memberBy propEq properties) repeatedProps

    -- Deactivate remomed or old properties
    forM_ (updatedProps ++ removedProps) $ \prop ->
        update_
            Table.properties
            ( \p ->
                p ! #requestId .== literal (P.requestId prop)
                    .&& p ! #name .== literal (P.name prop)
            )
            (\p -> p `with` [#active := false])

    -- Insert new properties
    let changedProps = filter (not . memberBy (flip propEq) unchangedProps) properties
    dt <- envIO now
    userId <- UI.userId <$> askUserInfo
    insert_ Table.properties $
        (\(name, value) -> P.Property def reqId userId name value dt True True) <$> changedProps

    -- Update the request
    update_ Table.requests (\r -> r ! #_id .== literal reqId) $
        \r -> r `with` [#title := literal title, #teamId := literal teamId]
  where
    propEq p (name, value) = P.name p == name && (P.value p == value)
    memberBy eq xs x = any (eq x) xs


-- TODO Refactor
updateResults :: EnvAction ()
updateResults = do
    properties <- getProps
    reqId <- param "_id"

    allProps <-
        query $
            select Table.properties `suchThat` \p ->
                p ! #active .== literal True .&& p ! #requestId .== literal reqId

    let (repeatedProps, removedProps) = partition (memberBy (\p (name, _) -> P.name p == name) properties) allProps
    let (unchangedProps, updatedProps) = partition (memberBy propEq properties) repeatedProps

    -- Deactivate remomed or old properties
    forM_ (updatedProps ++ removedProps) $ \prop ->
        update_
            Table.properties
            ( \p ->
                p ! #requestId .== literal (P.requestId prop)
                    .&& p ! #name .== literal (P.name prop)
            )
            (\p -> p `with` [#active := false])

    -- Insert new properties
    let changedProps = filter (not . memberBy (flip propEq) unchangedProps) properties
    dt <- envIO now
    userId <- UI.userId <$> askUserInfo
    insert_ Table.properties $
        (\(name, value) -> P.Property def reqId userId name value dt True True) <$> changedProps
  where
    propEq p (name, value) = P.name p == name && (P.value p == value)
    memberBy eq xs x = any (eq x) xs


getRequestAndProps :: (Value -> Parser a) -> EnvAction (a, [(Text, Text)])
getRequestAndProps p = do
    reqValue <- jsonParam "request" (key "request")
    propsValue <- jsonParam "properties" (key "properties")
    request <- runParser p reqValue
    properties <- runParser P.parseProperties propsValue
    return (request, properties)


getProps :: EnvAction [(Text, Text)]
getProps = jsonParam "properties" (key "properties") >>= runParser P.parseProperties


addComment :: EnvAction ()
addComment = do
    reqId <- param "_id"
    userId <- UI.userId <$> askUserInfo
    dt <- envIO now
    content <- jsonParamText "content"
    commentId <- insert Table.comments [C.Comment def reqId userId content dt]
    success $ C.Comment (toId commentId) reqId userId content dt
    status created201


updateStatus :: EnvAction ()
updateStatus = do
    reqId <- param "_id"
    status <- read . unpack <$> jsonParamText "status"
    update_ Table.requests (\r -> r ! #_id .== literal reqId) $
        \r -> r `with` [#status := literal status]


createWithProps :: EnvAction ()
createWithProps = do
    ((title, teamId, requestType), properties) <- getRequestAndProps R.parseRequestCreation
    dt <- envIO now
    userId <- UI.userId <$> askUserInfo
    let newReq = R.Request def title userId teamId Pending requestType dt
    reqId <- insertWithPK Table.requests [newReq]
    insert_ Table.properties $
        fmap (\(name, value) -> P.Property def reqId userId name value dt False True) properties
    success $ newReq{R._id = reqId}
    status created201
