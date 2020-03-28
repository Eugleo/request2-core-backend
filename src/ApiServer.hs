{-# LANGUAGE OverloadedStrings #-}

module ApiServer where

import qualified API.Announcement as Ann
import qualified API.Team as Team
import qualified API.User as User
import Capability
import Config
import Control.Monad
import qualified Data.Text as T
import Environment
import Model.User (Role (..))
import Network.Wai
import Web.Scotty (delete, get, post, put, scotty)
import qualified Web.Scotty as S
  ( function,
    json,
    middleware,
    notFound,
    options,
    text,
  )

addCORSHeader :: Middleware
addCORSHeader =
  modifyResponse . mapResponseHeaders $
    (++)
      [ ("Access-Control-Allow-Origin", "*"),
        ("Access-Control-Allow-Headers", "*"),
        ("Access-Control-Allow-Methods", "*")
      ]

apiServer :: ServerConfig -> IO ()
apiServer config =
  scotty (_listenPort config) $ do
    when (_allowCORS config) $ do
      S.middleware addCORSHeader
      S.options (S.function $ const $ Just []) $ S.text "CORS OK"
    {-
     - Capabilities
     -}
    get "/capability" $ S.json (capabilityList :: [T.Text])
    {-
     - Users
     -}
    post "/register-init" $ withDB User.mailRegToken
    post "/register" $ withDB User.register
    post "/login" $ withDB User.login
    post "/logout" $ withAuth User.logout
    post "/password" $ withAuth User.changePassword
    {-
     - User information
     -}
    get "/me" $ withAuth User.getDetails
    -- TODO put "/userinfo" $ withAuth undefined
    {-
     - Announcements
     -}
    get "/announcements" $ withAuth Ann.getAll
    get "/announcement/:ann_id" $ withAuth Ann.get
    post "/announcements" $ withRoles [Admin] Ann.create
    put "/announcement/:ann_id" $ withRoles [Admin] Ann.edit
    delete "/announcement/:ann_id" $ withRoles [Admin] Ann.deactivate
    {-
     - Teams
     -}
    get "/teams" $ withRoles [Admin] Team.getMany
    get "/teams/:team_id" $ withRoles [Admin] Team.get
    post "/teams" $ withRoles [Admin] Team.add
    put "/teams/:team_id" $ withRoles [Admin] Team.edit
    delete "/teams/:team_id" $ withRoles [Admin] Team.deactivate
    {-
     - Standard 404 -- keep this last
     -}
    S.notFound $ S.text "Not found"
  where
    withDB = withDBEnv config
    withAuth = withAuthEnv config
    withRoles = withRolesEnv config
