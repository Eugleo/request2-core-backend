{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.User where

import Api.Common (failure, success)
import Api.Query.Runner (runQuery)
import Api.Query.User
import Control.Monad (forM, unless, void, when)
import Data.Aeson (object, toJSON, (.=))
import Data.Environment
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Member (Member (Member))
import Data.Model.ApiKey (ApiKey (ApiKey), key)
import Data.Model.DateTime (DateTime (..), now)
import Data.Model.Role (Role (..))
import Data.Model.SecurityToken (SecurityToken (SecurityToken))
import Data.Model.Team (Team)
import Data.Model.User (User (..))
import qualified Data.Model.User as Us
import Data.Model.UserWithTeam (outerToInner)
import qualified Data.Model.UserWithTeam as OUWT (User (..))
import qualified Data.Text.IO as T
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Data.UserDetails (UserDetails (UserDetails))
import Data.UserInfo (UserInfo (UserInfo))
import qualified Data.UserInfo as U
import qualified Data.UserWithoutId as User
import qualified Data.UserWithoutIdWithTeam as OuterUser
import Database.Common (create, get)
import Database.Selda hiding (text, update)
import qualified Database.Table as Table
import Network.HTTP.Types.Status (badRequest400, created201, forbidden403, internalServerError500)
import qualified Server.Config as Cfg
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


logoutEverywhere :: ID Us.User -> EnvAction ()
logoutEverywhere userId =
    deleteFrom_ Table.apiKeys (\v -> v ! #userId .== literal userId)


editMe :: EnvAction ()
editMe = do
    ui <- askUserInfo
    name <- jsonParamText "name"
    update_
        Table.users
        (\u -> u ! #_id .== literal (U.userId ui))
        (\u -> u `with` [#name := literal name])
        `rescue` \_ ->
            failure "Can't update the user at the moment" internalServerError500


changePassword :: EnvAction ()
changePassword = do
    ui <- askUserInfo
    oldpass <- jsonParamText "old"
    newpass <- jsonParamText "new"
    match <- checkPassword (U.userId ui) oldpass
    if match
        then setPassword (U.userId ui) newpass
        else failure "The old password is incorrect" forbidden403


removeInvalidTokens :: EnvAction ()
removeInvalidTokens = do
    token <- jsonParamText "token"
    email <- jsonParamText "email"
    dt <- envIO now
    void $
        deleteFrom Table.securityTokens $ \t ->
            t ! #validUntil .< literal dt
                .|| (t ! #token .== literal token .&& t ! #email .== literal email)


checkToken :: EnvAction ()
checkToken = do
    token <- jsonParamText "token"
    email <- jsonParamText "email"
    currentDt <- envIO now
    tokenCheck <-
        query $
            select Table.securityTokens `suchThat` \t ->
                t ! #token .== literal token
                    .&& t ! #email .== literal email
                    .&& t ! #validUntil .> literal currentDt
    removeInvalidTokens
    when (length tokenCheck /= 1) $
        failure
            "The security token is invalid, please repeat the registration process"
            forbidden403


resetPassword :: EnvAction ()
resetPassword = do
    checkToken
    email <- jsonParamText "email"
    newPassword <- getPasswordHash
    let updater u = u `with` [#password := literal newPassword]
    update_ Table.users (\u -> u ! #email .== literal email) updater `rescue` \_ -> do
        text "Failed to create the new user entry"
        status internalServerError500 >> finish
    users <- query $ do
        user <- select Table.users `suchThat` \u -> u ! #email .== literal email
        return $ user ! #_id
    logoutEverywhere $ head users


checkEmailUnique :: EnvAction ()
checkEmailUnique = do
    email <- jsonParamText "email"
    others <- query $ select Table.users `suchThat` \u -> u ! #email .== literal email
    unless (null others) $ failure "User with this email already exists" badRequest400


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


getPasswordHash :: EnvAction Text
getPasswordHash = do
    passwordHash <- newHash <$> jsonParamText "password"
    envIO passwordHash


sendRegToken :: EnvAction ()
sendRegToken = do
    checkEmailUnique
    email <- jsonParamText "email"
    cfg <- askConfig
    token <- makeToken hours12
    let address = Address (Just "Some random name") email
    let link = makeLink [Cfg._frontendUrlBase cfg, "#", "register", email, token]
    envIO $ T.putStrLn $ "Sending registration link: " <> link
    envIO $ registrationInitMail cfg address link >>= sendmail' cfg


createNew :: EnvAction ()
createNew = do
    checkEmailUnique
    user <- jsonData :: EnvAction OuterUser.UserWithoutId
    password <- getPasswordHash
    u <-
        User.User (OuterUser.email user)
            <$> pure password
            <*> pure (OuterUser.name user)
            <*> pure (OuterUser.roles user)
            <*> envIO now
            <*> pure True
    newuser <-
        create Table.users u `rescue` \_ ->
            failure "Failed to create the new user entry" internalServerError500
    insert_ Table.member $ Member (Us._id newuser) <$> OuterUser.teamIds user
    success newuser
    status created201


getUser :: EnvAction ()
getUser = do
    userId <- param "_id" :: EnvAction (ID Us.User)
    user <- get Table.users userId
    teams <- getUserTeams userId
    case user of
        Nothing -> failure "Invalid user id" badRequest400
        Just u -> success $ innerToOuter u teams


innerToOuter :: Us.User -> [ID Team] -> OUWT.User
innerToOuter Us.User{_id, email, password, name, roles, dateCreated, active} teams =
    OUWT.User _id email password name roles teams dateCreated active


getUserTeams :: ID Us.User -> EnvAction [ID Team]
getUserTeams userId = query $ do
    mbr <- select Table.member `suchThat` \m -> m ! #userId .== literal userId
    return $ mbr ! #teamId


getUsers :: EnvAction ()
getUsers = do
    users <- runQuery Table.users userQueryTranslator
    usersWithteams <- forM users $ \u -> innerToOuter u <$> getUserTeams (Us._id u)
    success (object ["values" .= toJSON usersWithteams, "total" .= length usersWithteams])


updateUser :: EnvAction ()
updateUser = do
    userId <- param "_id"
    user <- jsonData :: EnvAction OUWT.User
    update_ Table.users (\u -> u ! #_id .== literal userId) $ \u ->
        row (outerToInner user) `with` [#password := u ! #password]
    -- Refresh the (Team, User) parinings
    deleteFrom_ Table.member $ \m -> m ! #userId .== literal userId
    insert_ Table.member $ fmap (Member userId) (OUWT.teamIds user)


hours12 :: Int
hours12 = 12 * 3600


makeToken :: Int -> EnvAction Text
makeToken expire = do
    dt <- envIO now
    email <- jsonParamText "email"
    token' <- regToken email <$> askConfig
    case token' of
        Nothing -> failure "The entered email is invalid" badRequest400
        Just token -> do
            insert_ Table.securityTokens [SecurityToken token email (add dt expire)]
            return token


makeLink :: [Text] -> Text
makeLink = fold . intersperse "/"


sendPwdResetEmail :: EnvAction ()
sendPwdResetEmail = do
    token <- makeToken hours12
    email <- jsonParamText "email"
    cfg <- askConfig
    maybeUsers <- query $ select Table.users `suchThat` \u -> u ! #email .== literal email
    let address = Address (Just "Some random name") email
    let link = makeLink [Cfg._frontendUrlBase cfg, "#", "password-reset", email, token]
    envIO $ T.putStrLn $ "Sending password reset link: " <> link
    envIO $ do
        mail <- case maybeUsers of
            [user] -> pwdResetMail cfg address (Us.name user) link
            _ -> userDoesNotExistMail cfg address
        sendmail' cfg mail


register :: EnvAction ()
register = do
    checkToken
    email <- jsonParamText "email"
    password <- getPasswordHash
    u <-
        User.User email
            <$> pure password
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
