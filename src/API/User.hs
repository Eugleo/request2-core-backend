module API.User where

import Crypto (regToken)
import qualified Data.Text.IO as T
import qualified Database.User as DB
import Environment
import qualified UserInfo as U

login :: EnvAction ()
login = do
  email <- jsonParam "email"
  password <- jsonParam "password"
  res <- DB.login email password
  case res of
    Just userInfo -> json userInfo
    Nothing -> envForbidden

logout :: EnvAction ()
logout = askUser >>= DB.logout . U.apiKey

changePassword :: EnvAction ()
changePassword = do
  user <- askUser
  oldpass <- jsonParam "old"
  newpass <- jsonParam "new"
  match <- DB.checkPassword (U.userID user) oldpass
  if match
    then DB.setPassword (U.userID user) newpass
    else envForbidden

getInfo :: EnvAction ()
getInfo = do
  user <- askUser
  json user

getDetails :: EnvAction ()
getDetails = do
  user <- askUser
  res <- DB.getDetails (U.userID user)
  case res of
    Just details -> json details
    Nothing -> envForbidden

mailRegToken :: EnvAction ()
mailRegToken = do
  eml <- jsonParam "email"
  tok' <- regToken eml <$> askConfig
  case tok' of
    Just tok -> do
      envIO . T.putStrLn $
        "Would send e-mail to " <> eml <> " with token " <> tok
      json $ ("ok" :: String)
    _ -> envBadRequest
