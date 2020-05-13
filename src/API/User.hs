{-# LANGUAGE OverloadedLabels #-}

module Api.User where

import Api.Common
import Crypto (checkHash, newHash, regToken)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Database.Schema as DB
import Database.Selda
import DateTime (now)
import Environment
import Model.User
import UserInfo (UserInfo)
import qualified UserInfo as U

-- login :: Text -> Text -> EnvAction (Maybe UserInfo)
-- login email password = do
--   db <- askDB
--   res <-
--     envIO $
--       query db "SELECT pw_hash, user_id FROM users WHERE email = ?" (Only email)
--   case res of
--     [(hash, userId)] ->
--       if checkHash password hash
--         then do
--           apiKey <- addApiKey userId
--           roles <- getRoles userId
--           -- TODO Log error on roles == Nothing
--           return . Just $ UserInfo userId apiKey (fromMaybe [] roles)
--         else return Nothing
--     _ -> return Nothing

login :: EnvAction ()
login = do
  email <- jsonParamText "email"
  password <- jsonParamText "password"
  res <- lift . lift . query $ do
    user <- select DB.users
    restrict (user ! #email .== literal email)
    return (user ! #password :*: user ! #_id)
  case res of
    [pwHash :*: userId] ->
      if checkHash password pwHash
        then do
          apiKey <- addApiKey userId
          roles <- getRoles userId
          -- TODO Log error on roles == Nothing
          return . Just $ UserInfo userId apiKey (fromMaybe [] roles)
        else return Nothing
    _ -> undefined
-- case res of
--   Just userInfo -> json userInfo
--   Nothing -> envForbidden

-- logout :: EnvAction ()
-- logout = askUser >>= DB.logout . U.apiKey

-- changePassword :: EnvAction ()
-- changePassword = do
--   user <- askUser
--   oldpass <- jsonParamText "old"
--   newpass <- jsonParamText "new"
--   match <- DB.checkPassword (U.userID user) oldpass
--   if match
--     then DB.setPassword (U.userID user) newpass
--     else envForbidden

-- getInfo :: EnvAction ()
-- getInfo = do
--   user <- askUser
--   json user

-- getDetails :: EnvAction ()
-- getDetails = do
--   user <- askUser
--   res <- DB.getDetails (U.userID user)
--   case res of
--     Just details -> json details
--     Nothing -> envServerError -- logged users should have retrievable info

-- mailRegToken :: EnvAction ()
-- mailRegToken = do
--   eml <- jsonParamText "email"
--   tok' <- regToken eml <$> askConfig
--   case tok' of
--     Just tok -> do
--       envIO
--         ( T.putStrLn $
--             "Sending e-mail to with activation link hash: #/register/"
--               <> eml
--               <> "/"
--               <> tok
--         )
--       envCreated
--     _ -> envBadRequest

-- -- TODO: check that the team actually exists
-- register :: EnvAction ()
-- register = do
--   eml <- jsonParamText "email"
--   tok <- jsonParamText "token"
--   tokCheck <- regToken eml <$> askConfig
--   when (Just tok /= tokCheck) $ envForbidden
--   password <- jsonParamText "password"
--   u <-
--     User eml <$> jsonParamText "name" <*> pure [Client] <*> jsonParamInt "team"
--       <*> envIO now
--   newuser <-
--     (envIO (newHash password) >>= Database.User.create u) `rescue` \_ -> do
--       text "Failed to create the new user entry"
--       envServerError
--   json newuser
--   envCreated
