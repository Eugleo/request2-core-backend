{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.User where

import Control.Monad (when)
import Data.Environment
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Model.ApiKey (ApiKey (ApiKey), key)
import Data.Model.DateTime (now)
import Data.Model.Role (Role (..))
import Data.Model.User (User (..))
import qualified Data.Model.User as Us
import qualified Data.Text.IO as T
import Data.UserDetails (UserDetails (UserDetails))
import Data.UserInfo (UserInfo (UserInfo))
import qualified Data.UserInfo as U
import qualified Data.UserWithoutId as User
import Database.Common
import Database.Selda hiding (text)
import qualified Database.Table as Table
import Network.HTTP.Types.Status (badRequest400, created201, forbidden403, internalServerError500, ok200)
import Utils.Crypto (checkHash, newApiKey, newHash, regToken)
import Utils.Mail.Common
import Utils.Mail.PwdResetMail


-- TODO is 403 the right status code here?
login :: EnvAction ()
login = do
    email <- jsonParamText "email"
    pw <- jsonParamText "password"
    res <- query $ select Table.users `suchThat` (\user -> user ! #email .== literal email)
    case res of
        [User{password, _id, roles}] ->
            if checkHash pw password
                then do
                    apiKey <- addApiKey _id
                    json $ UserInfo _id (key apiKey) roles
                else status forbidden403 >> finish
        _ -> status forbidden403 >> finish


addApiKey :: ID User -> EnvAction ApiKey
addApiKey userId = do
    (userApiKey, dbApiKey) <- liftIO newApiKey
    time <- liftIO now
    let ak = ApiKey dbApiKey userId time
    insert_ Table.apiKeys [ak]
    return $ ApiKey userApiKey userId time


logout :: EnvAction ()
logout = do
    ui <- askUserInfo
    deleteFrom_ Table.apiKeys (\v -> v ! #key .== literal (U.apiKey ui))


logoutEverywhere :: EnvAction ()
logoutEverywhere = do
    ui <- askUserInfo
    deleteFrom_ Table.apiKeys (\v -> v ! #userId .== literal (U.userId ui))


changePassword :: EnvAction ()
changePassword = do
    ui <- askUserInfo
    oldpass <- jsonParamText "old"
    newpass <- jsonParamText "new"
    match <- checkPassword (U.userId ui) oldpass
    if match
        then setPassword (U.userId ui) newpass
        else status forbidden403 >> finish


checkPassword :: ID User -> Text -> EnvAction Bool
checkPassword userId pw = do
    res <- query $ select Table.users `suchThat` (\user -> user ! #_id .== literal userId)
    return $
        case res of
            [User{password}] -> checkHash pw password
            _ -> False


setPassword :: ID User -> Text -> EnvAction ()
setPassword userId password = do
    pwHash <- envIO $ newHash password
    update_
        Table.users
        (\u -> u ! #_id .== literal userId)
        (\u -> u `with` [#password := literal pwHash])


getInfo :: EnvAction ()
getInfo = do
    ui <- askUserInfo
    json ui


getDetails :: EnvAction ()
getDetails = do
    ui <- askUserInfo
    res <- query $ select Table.users `suchThat` (\user -> user ! #_id .== literal (U.userId ui))
    case res of
        [User{_id, teamId, name, roles, dateCreated}] -> do
            maybeTeam <- get Table.teams teamId
            case maybeTeam of
                Just team -> json $ UserDetails _id name roles team dateCreated
                Nothing -> status internalServerError500 >> finish
        _ -> status internalServerError500 >> finish


mailRegToken :: EnvAction ()
mailRegToken = do
    eml <- jsonParamText "email"
    tok' <- regToken eml <$> askConfig
    case tok' of
        Just tok -> do
            envIO
                ( T.putStrLn $
                    "Sending e-mail to with activation link hash: #/register/"
                        <> eml
                        <> "/"
                        <> tok
                )
            status created201
        _ -> status badRequest400 >> finish


createNew :: EnvAction ()
createNew = do
    user <- jsonData
    let passwordHash = newHash $ User.password user
    u <-
        User.User (User.email user)
            <$> envIO passwordHash
            <*> pure (User.name user)
            <*> pure (User.roles user)
            <*> pure (User.teamId user)
            <*> pure (User.dateCreated user)
            <*> pure (User.active user)
    newuser <-
        create Table.users u `rescue` \_ -> do
            text "Failed to create the new user entry"
            status internalServerError500 >> finish
    json newuser
    status created201


-- TODO Change localhost to the correct one
sendPwdResetEmail :: EnvAction ()
sendPwdResetEmail = do
    email <- param "email"
    cfg <- askConfig
    let token' = regToken email cfg
    case token' of
        Nothing -> status badRequest400 >> finish
        Just token ->
            do
                maybeUsers <- query $ select Table.users `suchThat` \u -> u ! #email .== literal email
                let address = Address (Just "Some random name") email
                let link =
                        fold $ intersperse "/" ["http://localhost:9080", "#", "password-reset", email, token]
                envIO $ do
                    mail <- case maybeUsers of
                        [user] -> pwdResetMail cfg address (Us.name user) link
                        _ -> userDoesNotExistMail cfg address
                    sendmail' cfg mail
                status
                    ok200


-- TODO: check that the team actually exists
register :: EnvAction ()
register = do
    email <- jsonParamText "email"
    token <- jsonParamText "token"
    tokenCheck <- regToken email <$> askConfig
    when (Just token /= tokenCheck) $ status forbidden403 >> finish
    passwordHash <- newHash <$> jsonParamText "password"
    u <-
        User.User email
            <$> envIO passwordHash
            <*> jsonParamText "name"
            <*> pure [Client]
            <*> fmap toId (jsonParamInt "team")
            <*> envIO now
            <*> pure True
    newuser <-
        create Table.users u `rescue` \_ -> do
            text "Failed to create the new user entry"
            status internalServerError500 >> finish
    json newuser
    status created201
