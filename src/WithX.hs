module WithX where

import Config
import Control.Monad (unless)
import Control.Monad.Trans.Reader (ask, local, runReaderT)
import Data.Text.Lazy (toStrict)
import Database.BasicAuth
import Environment
import Model.Role (Role)
import Network.HTTP.Types
import UserInfo
import Web.Scotty.Trans hiding (json, jsonData, param, rescue, status, text)

withDBEnv :: ServerConfig -> EnvAction a -> Action a
withDBEnv config ea = runReaderT ea $ Env config Nothing

withAuthEnv :: ServerConfig -> EnvAction a -> Action a
withAuthEnv config = withDBEnv config . authentized

authentized :: EnvAction a -> EnvAction a
authentized action = do
  maybeApikey <- lift $ header "Authorization"
  case maybeApikey of
    Just userApiKey -> do
      auth <- findApiKeyUser (toStrict userApiKey)
      case auth of
        Just u -> local (\env -> env {envUser = Just u}) action
        _ -> status unauthorized401 >> lift finish
    Nothing -> status unauthorized401 >> lift finish

withRolesEnv :: ServerConfig -> [Role] -> EnvAction a -> Action a
withRolesEnv config rs action =
  withAuthEnv config $ do
    userRoles <- roles <$> askUser
    unless
      (all (`elem` userRoles) rs)
      (status forbidden403 >> lift finish)
    action

askUser :: EnvAction UserInfo
askUser = do
  mu <- envUser <$> ask
  case mu of
    Just u -> return u
    _ -> status forbidden403 >> lift finish
