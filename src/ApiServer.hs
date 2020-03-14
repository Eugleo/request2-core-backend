{-# LANGUAGE OverloadedStrings #-}

module ApiServer where

import qualified API.Announcement as Ann
import qualified Auth --TODO move to API because now it's really API
import qualified Data.Text as T
import Capability
import Config
import Control.Monad
import Model.User (Role(..))
import UserInfo
import Network.Wai.Middleware.Cors
import Web.Scotty
import Environment

apiServer :: ServerConfig -> IO ()
apiServer config =
  scotty (listenPort config) $ do
    when (allowCORS config) $ middleware simpleCors
      {-
       - Capabilities
       -}
    get "/capability" $ json (capabilityList :: [T.Text])
      {-
       - Users
       -}
    --post "/register" undefined
    --post "/register-verify/:token" undefined
    post "/login" $ withDBEnv config Auth.login
    --post "/logout" $ Auth.logout config
    --post "/password" $ auth Auth.changePassword
      {-
       - User information
       -}
    --get "/userinfo" $ auth undefined --TODO get user info
    --put "/userinfo" $ auth undefined --TODO get user info

      {-
       - Announcements -- testing without login requierd
       -}
    get "/announcements" $ withDBEnv config Ann.getAll
    get "/announcement/:ann_id" $ withDBEnv config Ann.get
      {-
       - Announcements
       -}
    --get "/announcements" $ withAuthEnv Ann.getAll
    --get "/announcement/:ann_id" $ withAuthEnv Ann.get
    --post "/announcements" $ withRoleEnv Admin Ann.create
    --put "/announcement/:ann_id" $ withRoleEnv Admin Ann.edit
    --delete "/announcement/:ann_id" $ withRoleEnv Admin Ann.deactivate
      {-
       - Standard 404 -- keep this last
       -}
    notFound $ text "Not found"
  where
    withRoleEnv  :: Role -> EnvAction a -> ActionM a
    withRoleEnv r m = withAuthEnv config $ checkRoles (r `elem`) >> m
