{-# LANGUAGE OverloadedStrings #-}

module ApiServer where

import qualified API.Announcement as Ann
import qualified API.User as User
import Capability
import Config
import Control.Monad
import qualified Data.Text as T
import Environment
import Model.User (Role (..))
import Network.Wai.Middleware.Cors
import Web.Scotty (delete, get, middleware, notFound, post, put, scotty)
import qualified Web.Scotty as S (json, text)

apiServer :: ServerConfig -> IO ()
apiServer config =
  scotty (listenPort config) $ do
    when (allowCORS config) $ middleware simpleCors
    {-
     - Capabilities
     -}
    get "/capability" $ S.json (capabilityList :: [T.Text])
    {-
     - Users
     -}
    --post "/register" withDB undefined
    --post "/register-verify/:token" withDB undefined
    post "/login" $ withDB User.login
    post "/logout" $ withAuth User.logout
    post "/password" $ withAuth User.changePassword
    {-
     - User information
     -}
    get "/userinfo" $ withAuth User.getUserInfo
    -- TODO put "/userinfo" $ withAuth undefined
    {-
     - Announcements
     -}
    get "/announcements" $ withAuth Ann.getAll
    get "/announcement/:ann_id" $ withAuth Ann.get
    post "/announcements" $ withRoles [Operator] Ann.create
    put "/announcement/:ann_id" $ withRoles [Operator] Ann.edit
    delete "/announcement/:ann_id" $ withRoles [Operator] Ann.deactivate
    {-
     - Standard 404 -- keep this last
     -}
    notFound $ S.text "Not found"
  where
    withDB = withDBEnv config
    withAuth = withAuthEnv config
    withRoles = withRolesEnv config
