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
import Network.Wai
import Web.Scotty (delete, get, middleware, notFound, post, put, scotty)
import qualified Web.Scotty as S (function, json, options, text)

addCORSHeader :: Middleware
addCORSHeader =
  modifyResponse $
    mapResponseHeaders
      ( [ ("Access-Control-Allow-Origin", "*"),
          ("Access-Control-Allow-Headers", "*")
        ]
          ++
      )

apiServer :: ServerConfig -> IO ()
apiServer config =
  scotty (listenPort config) $ do
    when (allowCORS config) $ do
      middleware addCORSHeader
      S.options (S.function $ const $ Just []) $ S.text "CORS OK"
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
    -- TODO Fix error with jsonData when there is no announcement
    get "/announcements" $ withAuth Ann.getAll
    get "/announcement/:ann_id" $ withAuth Ann.get
    post "/announcements" $ withRoles [Admin] Ann.create
    put "/announcement/:ann_id" $ withRoles [Admin] Ann.edit
    delete "/announcement/:ann_id" $ withRoles [Admin] Ann.deactivate
    {-
     - Standard 404 -- keep this last
     -}
    notFound $ S.text "Not found"
  where
    withDB = withDBEnv config
    withAuth = withAuthEnv config
    withRoles = withRolesEnv config
