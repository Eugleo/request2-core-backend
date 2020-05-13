{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ApiServer where

import qualified Api.Common as Api
import qualified Api.User as User
import Capability
import Config
import Control.Monad
import qualified Data.Text as T
import qualified Database.Schema as DB
import Database.Selda.PostgreSQL (PGConnectInfo (..), withPostgreSQL)
import Model.AnnWithoutId (AnnWithoutId)
import Model.Role (Role (..))
import Model.TeamWithoutId (TeamWithoutId)
import Network.Wai
import qualified Web.Scotty.Trans as S
  ( function,
    json,
    middleware,
    notFound,
    options,
    text,
  )
import Web.Scotty.Trans (delete, get, post, put, scottyT)
import WithX

-- TODO Replace with info from config
connInfo :: PGConnectInfo
connInfo = PGConnectInfo "localhost" 5432 "request" Nothing (Just "eugen") Nothing

addCORSHeader :: Middleware
addCORSHeader =
  modifyResponse . mapResponseHeaders $
    (++)
      [ ("Access-Control-Allow-Origin", "*"),
        ("Access-Control-Allow-Headers", "*"),
        ("Access-Control-Allow-Methods", "*")
      ]

apiServer :: ServerConfig -> IO ()
apiServer config = scottyT (_listenPort config) (withPostgreSQL connInfo)
  $ when (_allowCORS config)
  $ do
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
    get "/announcements" $ withAuth $ Api.getMany DB.anns
    get "/announcement/:_id" $ withAuth $ Api.get DB.anns
    post "/announcements" $ withRoles [Admin] $ Api.create @AnnWithoutId DB.anns
    -- put "/announcement/:ann_id" $ withRoles [Admin] Ann.edit
    delete "/announcement/:_id" $ withRoles [Admin] $ Api.deactivate DB.anns
    {-
     - Teams
     -}
    get "/teams" $ withRoles [Admin] $ Api.getMany DB.teams
    get "/teams/:_id" $ withRoles [Admin] $ Api.get DB.teams
    post "/teams" $ withRoles [Admin] $ Api.create @TeamWithoutId DB.teams
    -- put "/teams/:team_id" $ withRoles [Admin] Team.edit
    delete "/teams/:_id" $ withRoles [Admin] $ Api.deactivate DB.teams
    {-
     - Standard 404 -- keep this last
     -}
    S.notFound $ S.text "Not found"
  where
    withDB = withDBEnv config
    withAuth = withAuthEnv config
    withRoles = withRolesEnv config
