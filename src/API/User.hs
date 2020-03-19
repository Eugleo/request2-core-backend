module API.User where

import qualified Database.User as DB
import Environment
import Network.HTTP.Types.Status
import qualified UserInfo as U

login :: EnvAction ()
login = do
  email <- jsonParam "email"
  password <- jsonParam "password"
  res <- DB.login email password
  case res of
    Just userInfo -> json userInfo
    Nothing -> status forbidden403

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
    else status forbidden403

getUserInfo :: EnvAction ()
getUserInfo = do
  user <- askUser
  json user
