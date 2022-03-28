{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Request where

import Api.Common (checkUserHasRoles, failure, get, notFound, success)
import Api.Query.Request (requestQueryTranslator)
import Api.Query.Runner
import Control.Lens (to, (^..), (^?), _Just)
import Control.Monad (forM_, unless)
import Data.Aeson (KeyValue ((.=)), Value, object, toJSON)
import Data.Aeson.Lens (AsPrimitive (_String), key, values, _JSON)
import Data.Environment (EnvAction, askUserInfo, envIO, fromJsonKey, jsonData, jsonParam, jsonParamText, param, rescue, status)
import Data.List (partition)
import qualified Data.Model.Comment as C
import Data.Model.DateTime (now)
import qualified Data.Model.Property as P
import Data.Model.Request (Request ())
import qualified Data.Model.Request as R
import Data.Model.Role (Role (..))
import Data.Model.Status (Status (Pending))
import Data.Model.Team (Team)
import Data.Text (pack, unpack)
import qualified Data.UserInfo as UI
import Database.Selda
import qualified Database.Table as Table
import Debug.Trace (traceShow)
import Network.HTTP.Types.Status (badRequest400, created201, forbidden403)


getWithProps :: EnvAction ()
getWithProps = do
    reqId <- param "_id"
    checkIsAuthorized reqId [Operator, Admin]
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
    checkIsAuthorized reqId [Operator, Admin]
    comments <- query $ select Table.comments `suchThat` \c -> c ! #requestId .== literal reqId
    success comments


getProperties :: EnvAction ()
getProperties = do
    reqId <- param "_id"
    checkIsAuthorized reqId [Operator, Admin]
    props <- query $ select Table.properties `suchThat` \c -> c ! #requestId .== literal reqId
    success props


-- TODO Add transactions
updateWithProps :: EnvAction ()
updateWithProps = do
    reqId <- param "_id"
    checkIsAuthorized reqId [Operator, Admin]
    ((title, teamId), properties) <- getRequestAndProps
    updateProperties reqId properties True

    -- Update the request
    update_ Table.requests (\r -> r ! #_id .== literal reqId) $
        \r -> r `with` [#title := literal title, #teamId := literal teamId]


updateProperties :: ID R.Request -> [(Text, Text)] -> Bool -> EnvAction ()
updateProperties reqId properties removeOthers = do
    allProps <-
        query $
            select Table.properties `suchThat` \p ->
                p ! #active .== literal True .&& p ! #requestId .== literal reqId

    let (repeatedProps, removedProps) =
            partition
                (memberBy (\p (name, _) -> P.name p == name) properties)
                allProps
    let (unchangedProps, updatedProps) = partition (memberBy propEq properties) repeatedProps

    -- Deactivate removed or old properties
    forM_ (updatedProps ++ if removeOthers then removedProps else []) $ \prop ->
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


updateResults :: EnvAction ()
updateResults = do
    reqId <- param "_id"
    properties <- getProps
    updateProperties reqId properties False


getMyRequests :: EnvAction ()
getMyRequests = do
    userId <- UI.userId <$> askUserInfo
    reqs <- runQuery Table.requests requestQueryTranslator
    let onlyMine = filter ((== userId) . R.authorId) reqs
    success $ object ["values" .= toJSON onlyMine, "total" .= length onlyMine]


getRequest :: EnvAction ()
getRequest = do
    reqId <- param "_id"
    checkIsAuthorized reqId [Operator, Admin]
    get Table.requests


getRequestAndProps :: EnvAction ((Text, ID Team), [(Text, Text)])
getRequestAndProps = do
    title <- jsonParam "request.title" (key "request" . key "title" . _JSON)
    teamId <- jsonParam "request.teamId" (key "request" . key "teamId" . _JSON)
    properties <- getProps
    return ((title, teamId), properties)


getProps = do
    (js :: Value) <-
        jsonData `rescue` (flip failure badRequest400 . ("Query JSON parsing error: " <>))
    case js ^.. (key "properties" . values . to toPropTuple . _Just) of
        [] -> failure "Missing or malformed parameter: properties" badRequest400
        things -> return things
  where
    toPropTuple js = do
        name <- js ^? key "name" . _String
        value <- js ^? key "value" . _String
        return (name, value)


checkIsAuthorized :: ID Request -> [Role] -> EnvAction ()
checkIsAuthorized reqId okRoles = do
    userId <- UI.userId <$> askUserInfo
    hasRoles <- checkUserHasRoles userId okRoles
    unless hasRoles $ do
        reqs <- query $ select Table.requests `suchThat` (\r -> r ! #_id .== literal reqId)
        case reqs of
            [request] ->
                unless (R.authorId request == userId) $
                    failure "You need to be the author, or an operator" forbidden403
            _ -> failure "Incorrect request ID" badRequest400


addComment :: EnvAction ()
addComment = do
    reqId <- param "_id"
    checkIsAuthorized reqId [Operator, Admin]
    userId <- UI.userId <$> askUserInfo
    dt <- envIO now
    content <- jsonParamText "content"
    commentId <- insert Table.comments [C.Comment def reqId userId content dt]
    success $ C.Comment (toId commentId) reqId userId content dt
    status created201


updateStatus :: EnvAction ()
updateStatus = do
    reqId <- param "_id"
    st <- read . unpack <$> jsonParamText "status"
    updateProperties reqId [("Status", pack . show $ st)] False
    update_ Table.requests (\r -> r ! #_id .== literal reqId) $
        \r -> r `with` [#status := literal st]


createWithProps :: EnvAction ()
createWithProps = do
    ((title, teamId), properties) <- getRequestAndProps
    requestType <- jsonParam "request.requestType" (key "request" . key "requestType" . _JSON)
    dt <- envIO now
    userId <- UI.userId <$> askUserInfo
    let newReq = R.Request def title userId teamId Pending requestType dt
    reqId <- insertWithPK Table.requests [newReq]
    insert_ Table.properties $
        fmap (\(name, value) -> P.Property def reqId userId name value dt False True) properties
    success $ newReq{R._id = reqId}
    status created201
