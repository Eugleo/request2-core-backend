module Utils.WithX where

import Control.Monad (unless)
import Control.Monad.Reader.Class (ask, local)
import Data.Environment
import Data.Model.Role (Role)
import Data.UserInfo
import Database.BasicAuth
import Network.HTTP.Types
import Server.Config

withDBEnv :: Config -> EnvAction a -> Action a
withDBEnv config ea = runEnvAction ea $ Env config Nothing

withAuthEnv :: Config -> EnvAction a -> Action a
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

withRolesEnv :: Config -> [Role] -> EnvAction a -> Action a
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
