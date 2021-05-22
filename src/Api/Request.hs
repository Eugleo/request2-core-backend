{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api.Request where

import Api.Common (notFound, success, failure, get)
import Api.Query.Request (requestQueryTranslator)
import Api.Query.Runner
import Control.Monad (forM_, unless)
import Data.Aeson (KeyValue ((.=)), object, toJSON)
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Parser, Value)
import Data.Environment (EnvAction, askUserInfo, envIO, jsonParam, jsonParamText, param, runParser, status)
import Data.List (partition)
import qualified Data.Model.Comment as C
import Data.Model.DateTime (now)
import qualified Data.Model.Property as P
import Data.Model.Request (parseStatus, Request())
import qualified Data.Model.Request as R
import Data.Model.Status (Status (Pending))
import Data.Text (unpack, pack)
import qualified Data.UserInfo as UI
import Data.Model.Role (Role(..))
import Data.Model.User (User(..))
import Database.Selda
import qualified Database.Table as Table
import Network.HTTP.Types.Status (created201, badRequest400, internalServerError500, forbidden403)


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
    ((title, teamId), properties) <- getRequestAndProps R.parseRequestEdit
    updateProperties reqId properties

    -- Update the request
    update_ Table.requests (\r -> r ! #_id .== literal reqId) $
        \r -> r `with` [#title := literal title, #teamId := literal teamId]

updateProperties :: ID R.Request -> [(Text, Text)] -> EnvAction ()
updateProperties reqId properties = do
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


updateResults :: EnvAction ()
updateResults = do
    reqId <- param "_id"
    properties <- getProps
    updateProperties reqId properties

getMyRequests :: EnvAction ()
getMyRequests = do
    userId <- UI.userId <$> askUserInfo
    reqs <- runQuery Table.requests requestQueryTranslator
    let onlyMine = filter ((== userId) .  R.authorId) reqs
    success $ object ["values" .= toJSON onlyMine, "total" .= length onlyMine]

getRequest :: EnvAction ()
getRequest = do
    reqId <- param ":_id"
    checkIsAuthorized reqId [Operator, Admin]
    get Table.requests


getRequestAndProps :: (Value -> Parser a) -> EnvAction (a, [(Text, Text)])
getRequestAndProps p = do
    reqValue <- jsonParam "request" (key "request")
    propsValue <- jsonParam "properties" (key "properties")
    request <- runParser p reqValue
    properties <- runParser P.parseProperties propsValue
    return (request, properties)


getProps :: EnvAction [(Text, Text)]
getProps = jsonParam "properties" (key "properties") >>= runParser P.parseProperties

checkIsAuthorized :: ID Request -> [Role] -> EnvAction ()
checkIsAuthorized reqId okRoles = do
    userId <- UI.userId <$> askUserInfo
    user <- query $ select Table.users `suchThat` (\u -> u ! #_id .== literal userId)
    case user of
        [User{roles}] ->
            unless (any (`elem` roles) okRoles) $ do
                reqs <- query $ select Table.requests `suchThat` (\r -> r ! #_id .== literal reqId)
                case reqs of
                    [request] ->
                        unless (R.authorId request == userId) $
                            failure "You need to be the author, or an operator" forbidden403
                    _ -> failure "Incorrect request ID" badRequest400
        _ -> failure "Incorrect user ID" badRequest400


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
    status <- read . unpack <$> jsonParamText "status"
    updateProperties reqId [("Status", pack . show $ status)]
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
