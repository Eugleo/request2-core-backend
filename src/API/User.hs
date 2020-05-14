{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Api.User where

import Api.Common
import ApiKey (ApiKey (ApiKey), key)
import Control.Monad (when)
import Crypto (checkHash, newApiKey, newHash, regToken)
import qualified Data.Text.IO as T
import Database.Selda hiding (text)
import qualified Database.Table as Table
import DateTime (now)
import Environment
import Model.Role (Role (..))
import Model.User (User)
import Model.UserDetails (UserDetails (UserDetails))
import qualified Model.UserWithoutId as User
import Network.HTTP.Types.Status (badRequest400, created201, forbidden403, internalServerError500)
import UserInfo (UserInfo (UserInfo))
import qualified UserInfo as U

-- TODO is 403 the right status code here?
login :: EnvAction ()
login = do
  email <- jsonParamText "email"
  password <- jsonParamText "password"
  res <- query $ do
    user <- select Table.users
    restrict (user ! #email .== literal email)
    return (user ! #password :*: user ! #_id :*: user ! #roles)
  case res of
    [pwHash :*: userId :*: roles] ->
      if checkHash password pwHash
        then do
          apiKey <- addApiKey userId
          json $ UserInfo userId (key apiKey) roles
        else status forbidden403 >> finish
    _ -> status forbidden403 >> finish

addApiKey :: ID User -> EnvAction ApiKey
addApiKey userId = do
  apiKey <- liftIO newApiKey
  time <- liftIO now
  let ak = ApiKey apiKey userId time
  insert_ Table.apiKeys [ak]
  return ak

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
checkPassword userId password = do
  res <- query $ do
    user <- select Table.users
    restrict (user ! #_id .== literal userId)
    return (user ! #password)
  return $
    case res of
      [hash] -> checkHash password hash
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
  res <- query $ do
    user <- select Table.users
    restrict (user ! #_id .== literal (U.userId ui))
    return $
      user ! #name
        :*: user ! #teamId
        :*: user ! #roles
        :*: user ! #dateCreated
  case res of
    [(name :*: teamId :*: roles :*: created)] -> do
      maybeTeam <- getBy Table.teams teamId
      case maybeTeam of
        Just team -> json $ UserDetails name roles team created
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
    createFrom Table.users u `rescue` \_ -> do
      text "Failed to create the new user entry"
      status internalServerError500 >> finish
  json newuser
  status created201
