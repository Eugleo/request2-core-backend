{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.User where

import Api.Common (failure, success)
import Api.Query.Runner (runQuery)
import Api.Query.User
import Control.Monad (forM, unless, void, when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Environment
import Data.Foldable (fold)
import qualified Data.HashMap.Lazy as HML
import Data.List (intersperse)
import Data.Member (Member (Member))
import Data.Model.ApiKey (ApiKey (ApiKey))
import qualified Data.Model.ApiKey as AK
import Data.Model.DateTime (DateTime (..), now)
import Data.Model.Role (Role (..))
import Data.Model.SecurityToken (SecurityToken (SecurityToken))
import Data.Model.Team (Team)
import Data.Model.User (User (..))
import qualified Data.Model.User as Us
import qualified Data.Text.IO as T
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Data.UserDetails (UserDetails (UserDetails))
import Data.UserInfo (UserInfo (UserInfo))
import qualified Data.UserInfo as U
import Database.Common (get)
import Database.Selda hiding (text, update)
import qualified Database.Table as Table
import Network.HTTP.Types.Status (badRequest400, created201, forbidden403, internalServerError500)
import Network.URI.Encode (encodeText)
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
                    success $ UserInfo _id (AK.key apiKey) roles
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


getName :: EnvAction ()
getName = do
    userId <- param "_id"
    res <- query $ select Table.users `suchThat` (\user -> user ! #_id .== literal userId)
    case res of
        [User{name}] -> json $ object ["data" .= object ["name" .= name]]
        _ -> failure "Incorrect user id supplied" badRequest400


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
    token <- makeToken "registration" hours12
    let address = Address (Just "Some random name") email
    let link = makeLink [Cfg._frontendUrlBase cfg, "#", "register", encodeText email, token]
    envIO $ T.putStrLn $ "Sending registration link: " <> link
    envIO $ registrationInitMail cfg address link >>= sendmail' cfg


register :: EnvAction ()
register = do
    checkToken
    void $ addNewUser [Client]


adminCreate :: EnvAction ()
adminCreate = do
    checkEmailUnique
    teamIds <- fromJsonKey "teamIds"
    roles <- fromJsonKey "roles"
    newuser <- addNewUser roles
    insert_ Table.member $ Member (Us._id newuser) <$> teamIds


addNewUser :: [Role] -> EnvAction User
addNewUser roles = do
    (email, name, room, telephone) <- getBasicUserInfo
    password <- getPasswordHash
    dateCreated <- envIO now
    let u =
            Us.User
                { _id = def,
                  email,
                  name,
                  room,
                  telephone,
                  password,
                  roles = roles,
                  dateCreated,
                  active = True
                }
    newuser <-
        addUser u `rescue` \_ -> do
            text "Failed to create the new user"
            status internalServerError500 >> finish
    success newuser
    status created201
    return newuser
  where
    addUser u = do
        rowId <- insertWithPK Table.users [u]
        return u{_id = rowId}


getUser :: EnvAction ()
getUser = do
    userId <- param "_id" :: EnvAction (ID Us.User)
    user <- get Table.users userId
    teams <- getUserTeams userId
    case user of
        Nothing -> failure "Invalid user id" badRequest400
        Just u -> success $ userWithTeams u teams


getUserTeams :: ID Us.User -> EnvAction [ID Team]
getUserTeams userId = query $ do
    mbr <- select Table.member `suchThat` \m -> m ! #userId .== literal userId
    return $ mbr ! #teamId


getUsers :: EnvAction ()
getUsers = do
    users <- runQuery Table.users userQueryTranslator
    usersWithteams <- forM users $ \u -> userWithTeams u <$> getUserTeams (Us._id u)
    success (object ["values" .= toJSON usersWithteams, "total" .= length usersWithteams])


userWithTeams :: Us.User -> [ID Team] -> Value
userWithTeams user teams = case toJSON user of
    Object o -> Object $ HML.insert "teamIds" (toJSON teams) o
    val -> val


getBasicUserInfo :: EnvAction (Text, Text, Text, Text)
getBasicUserInfo = do
    email <- fromJsonKey "email"
    name <- fromJsonKey "name"
    room <- fromJsonKey "room"
    telephone <- fromJsonKey "telephone"
    return (email, name, room, telephone)


adminUpdate :: EnvAction ()
adminUpdate = do
    userId <- param "_id"
    active <- fromJsonKey "active"
    roles <- fromJsonKey "roles"
    teamIds <- fromJsonKey "teamIds"
    (email, name, room, telephone) <- getBasicUserInfo
    update_ Table.users (\u -> u ! #_id .== literal userId) $ \u ->
        u
            `with` [ #email := literal email,
                     #name := literal name,
                     #room := literal room,
                     #telephone := literal telephone,
                     #active := literal active,
                     #roles := literal roles
                   ]
    -- Refresh the (Team, User) parinings
    deleteFrom_ Table.member $ \m -> m ! #userId .== literal userId
    insert_ Table.member $ fmap (Member userId) teamIds


hours12 :: Int
hours12 = 12 * 3600


makeToken :: Text -> Int -> EnvAction Text
makeToken reason expire = do
    dt <- envIO now
    email <- jsonParamText "email"
    token' <- regToken (reason <> email) <$> askConfig
    case token' of
        Nothing -> failure "The entered email is invalid" badRequest400
        Just token -> do
            let expiryDate = add dt expire
            _ <-
                Data.Environment.upsert
                    Table.securityTokens
                    (\t -> t ! #email .== literal email .&& t ! #token .== literal token)
                    (\t -> t `with` [#validUntil := literal expiryDate])
                    [SecurityToken token email expiryDate]
            return token


makeLink :: [Text] -> Text
makeLink = fold . intersperse "/"


sendPwdResetEmail :: EnvAction ()
sendPwdResetEmail = do
    token <- makeToken "pwd_reset" hours12
    email <- jsonParamText "email"
    cfg <- askConfig
    maybeUsers <- query $ select Table.users `suchThat` \u -> u ! #email .== literal email
    let address = Address (Just "Some random name") email
    let link = makeLink [Cfg._frontendUrlBase cfg, "#", "password-reset", encodeText email, token]
    envIO $ T.putStrLn $ "Sending password reset link: " <> link
    envIO $ do
        mail <- case maybeUsers of
            [user] -> pwdResetMail cfg address (Us.name user) link
            _ -> userDoesNotExistMail cfg address
        sendmail' cfg mail
