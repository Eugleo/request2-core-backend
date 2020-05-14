module WithX where

import Config
import Control.Monad (unless)
import Control.Monad.Reader.Class (ask, local)
import Database.BasicAuth
import Environment
import Model.Role (Role)
import Network.HTTP.Types
import UserInfo

withDBEnv :: ServerConfig -> EnvAction a -> Action a
withDBEnv config ea = runEnvAction ea $ Env config Nothing

withAuthEnv :: ServerConfig -> EnvAction a -> Action a
withAuthEnv config = withDBEnv config . authentized

authentized :: EnvAction a -> EnvAction a
authentized action = do
  maybeApikey <- header "Authorization"
  case maybeApikey of
    Just userApiKey -> do
      auth <- findApiKeyUser userApiKey
      case auth of
        Just u -> local (\env -> env {envUser = Just u}) action
        _ -> status unauthorized401 >> finish
    Nothing -> status unauthorized401 >> finish

withRolesEnv :: ServerConfig -> [Role] -> EnvAction a -> Action a
withRolesEnv config rs action =
  withAuthEnv config $ do
    userRoles <- roles <$> askUser
    unless
      (all (`elem` userRoles) rs)
      (status forbidden403 >> finish)
    action

askUser :: EnvAction UserInfo
askUser = do
  mu <- envUser <$> ask
  case mu of
    Just u -> return u
    _ -> status forbidden403 >> finish
