{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.User where

import Api.Common (failure, success)
import Api.Query.Runner (runQuery)
import Api.Query.User
import Control.Monad (forM, forM_, when)
import Data.Aeson (object, toJSON, (.=))
import Data.Environment
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Member (Member (Member))
import Data.Model.ApiKey (ApiKey (ApiKey), key)
import Data.Model.DateTime (DateTime (..), now)
import Data.Model.Role (Role (..))
import Data.Model.SecurityToken (SecurityToken (SecurityToken))
import Data.Model.User (User (..))
import qualified Data.Model.User as Us
import qualified Data.Model.UserWithTeam as OUWT (User (..))
import qualified Data.Text.IO as T
import Data.Time (UTCTime (UTCTime), nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Data.UserDetails (UserDetails (UserDetails))
import Data.UserInfo (UserInfo (UserInfo))
import qualified Data.UserInfo as U
import qualified Data.UserWithoutId as User
import qualified Data.UserWithoutIdWithTeam as OuterUser
import Database.Common (create, get, update)
import Database.Selda hiding (text, update)
import qualified Database.Table as Table
import Network.HTTP.Types.Status (badRequest400, created201, forbidden403, internalServerError500, ok200)
import Utils.Crypto (checkHash, newApiKey, newHash, regToken)
import Utils.Mail.Common
import Utils.Mail.PwdResetMail
import Utils.Mail.RegistrationMail


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
                    success $ UserInfo _id (key apiKey) roles
                else failure "Incorrect email or password" forbidden403
        _ -> failure "Incorrect email or password" forbidden403


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
        [User{_id, name, roles, dateCreated}] -> do
            teams <- query $ do
                mbr <- select Table.member `suchThat` \m -> m ! #userId .== literal _id
                innerJoin (\t -> t ! #_id .== mbr ! #teamId) $ select Table.teams
            success $ UserDetails _id name roles teams dateCreated
        _ -> failure "Incorrect user id supplied" badRequest400


add :: DateTime -> Int -> DateTime
add (DateTime dt) t =
    DateTime
        . posixSecondsToUTCTime
        . secondsToNominalDiffTime
        . fromIntegral
        . (t +)
        . round
        . nominalDiffTimeToSeconds
        . utcTimeToPOSIXSeconds
        $ dt


-- TODO Change localhost to the correct one
sendRegToken :: EnvAction ()
sendRegToken = do
    email <- jsonParamText "email"
    others <- query $ select Table.users `suchThat` \u -> u ! #email .== literal email
    case others of
        [] -> do
            cfg <- askConfig
            tok' <- regToken email <$> askConfig
            case tok' of
                Just token -> do
                    currentDt <- envIO now
                    -- Add approx. 12h to the current datetime
                    insert_ Table.securityTokens [SecurityToken token email (add currentDt 43200)]
                    let address = Address (Just "Some random name") email
                    let link =
                            fold $ intersperse "/" ["http://localhost:3000", "#", "register", email, token]
                    envIO $ T.putStrLn $ "Sending registration link: " <> link
                    envIO $ do
                        mail <- registrationInitMail cfg address link
                        sendmail' cfg mail
                    status ok200
                _ -> failure "The entered email is invalid" badRequest400
        _ -> failure "User with this email already exists" badRequest400


createNew :: EnvAction ()
createNew = do
    user <- jsonData :: EnvAction OuterUser.UserWithoutId
    let passwordHash = newHash $ OuterUser.password user
    u <-
        User.User (OuterUser.email user)
            <$> envIO passwordHash
            <*> pure (OuterUser.name user)
            <*> pure (OuterUser.roles user)
            <*> pure (OuterUser.dateCreated user)
            <*> pure (OuterUser.active user)
    newuser <-
        create Table.users u `rescue` \_ ->
            failure "Failed to create the new user entry" internalServerError500
    forM_ (OuterUser.teamIds user) $ \teamId ->
        insert_ Table.member [Member (Us._id newuser) teamId]
    success newuser
    status created201


getUser :: EnvAction ()
getUser = do
    userId <- param "_id" :: EnvAction (ID Us.User)
    user <- get Table.users userId
    teams <- query $ do
        mbr <- select Table.member `suchThat` \m -> m ! #userId .== literal userId
        return $ mbr ! #teamId
    case user of
        Nothing -> failure "Invalid user id" badRequest400
        Just u ->
            success $
                OUWT.User
                    userId
                    (Us.email u)
                    (Us.password u)
                    (Us.name u)
                    (Us.roles u)
                    teams
                    (Us.dateCreated u)
                    (Us.active u)


getUsers :: EnvAction ()
getUsers = do
    users <- runQuery Table.users userQueryTranslator
    usersWithteams <- forM users $ \u -> do
        let userId = Us._id u
        teams <- query $ do
            mbr <- select Table.member `suchThat` \m -> m ! #userId .== literal userId
            return $ mbr ! #teamId
        return $
            OUWT.User
                userId
                (Us.email u)
                (Us.password u)
                (Us.name u)
                (Us.roles u)
                teams
                (Us.dateCreated u)
                (Us.active u)
    success (object ["values" .= toJSON usersWithteams, "total" .= length usersWithteams])


updateUser :: EnvAction ()
updateUser = do
    userId <- param "_id" :: EnvAction (ID Us.User)
    user <- jsonData :: EnvAction OUWT.User
    pwd <- query $ do
        u <- select Table.users
        restrict $ u ! #_id .== literal userId
        return $ u ! #password
    case pwd of
        [password] -> do
            u <-
                Us.User userId
                    <$> pure (OUWT.email user)
                    <*> pure password
                    <*> pure (OUWT.name user)
                    <*> pure (OUWT.roles user)
                    <*> pure (OUWT.dateCreated user)
                    <*> pure (OUWT.active user)
            update Table.users userId u
            deleteFrom_ Table.member $ \m -> m ! #userId .== literal userId
            insert_ Table.member $ fmap (Member userId) (OUWT.teamIds user)
        _ -> failure "Two users with the same ID" internalServerError500


-- TODO Change localhost to the correct one
sendPwdResetEmail :: EnvAction ()
sendPwdResetEmail = do
    email <- jsonParamText "email"
    cfg <- askConfig
    let token' = regToken email cfg
    case token' of
        Nothing -> status badRequest400 >> finish
        Just token -> do
            maybeUsers <- query $ select Table.users `suchThat` \u -> u ! #email .== literal email
            let address = Address (Just "Some random name") email
            let link =
                    fold $
                        intersperse "/" ["http://localhost:3000", "#", "password-reset", email, token]
            envIO $ T.putStrLn $ "Sending password reset link: " <> link
            envIO $ do
                mail <- case maybeUsers of
                    [user] -> pwdResetMail cfg address (Us.name user) link
                    _ -> userDoesNotExistMail cfg address
                sendmail' cfg mail
            status ok200


register :: EnvAction ()
register = do
    email <- jsonParamText "email"
    token <- jsonParamText "token"
    currentDt <- envIO now
    tokenCheck' <-
        query $
            select Table.securityTokens `suchThat` \t ->
                t ! #token .== literal token
                    .&& t ! #email .== literal email
                    .&& t ! #validUntil .> literal currentDt

    case tokenCheck' of
        [_] -> do
            passwordHash <- newHash <$> jsonParamText "password"
            u <-
                User.User email
                    <$> envIO passwordHash
                    <*> jsonParamText "name"
                    <*> pure [Client]
                    <*> envIO now
                    <*> pure True
            newuser <-
                create Table.users u `rescue` \_ -> do
                    text "Failed to create the new user entry"
                    status internalServerError500 >> finish
            success newuser
            status created201
        _ -> do
            _ <- deleteFrom Table.securityTokens $ \t ->
                t ! #validUntil .< literal currentDt
                    .|| (t ! #token .== literal token .&& t ! #email .== literal email)
            failure
                "The security token is invalid, please repeat the registration process"
                forbidden403
