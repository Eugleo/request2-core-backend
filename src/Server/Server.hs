{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Server.Server where

import qualified Api.Common as Api
import qualified Api.Request as Request
import qualified Api.User as User
import Control.Monad
import Data.AnnWithoutId (AnnWithoutId)
import Data.Model.Role (Role (..))
import Data.TeamWithoutId (TeamWithoutId)
import qualified Data.Text as T
import Database.Selda.PostgreSQL (PGConnectInfo (..), on, withPostgreSQL)
import qualified Database.Table as Table
import Network.Wai
import Server.Capability
import Server.Config
import Utils.WithX
import qualified Web.Scotty.Trans as S
  ( function,
    json,
    middleware,
    notFound,
    options,
    text,
  )
import Web.Scotty.Trans (delete, get, post, put, scottyT)

-- TODO Replace with info from config
connInfo :: PGConnectInfo
connInfo = "request" `on` "localhost"

addCORSHeader :: Middleware
addCORSHeader =
  modifyResponse . mapResponseHeaders $
    (++)
      [ ("Access-Control-Allow-Origin", "*"),
        ("Access-Control-Allow-Headers", "*"),
        ("Access-Control-Allow-Methods", "*")
      ]

server :: Config -> IO ()
server config = scottyT (_listenPort config) (withPostgreSQL connInfo)
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
    get "/users/:_id" $ withRoles [Admin, Operator] $ Api.get Table.users
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
    get "/announcements" $ withAuth $ Api.getMany Table.anns
    get "/announcements/:_id" $ withAuth $ Api.get Table.anns
    post "/announcements" $ withRoles [Admin] $ Api.create @AnnWithoutId Table.anns
    put "/announcements/:_id" $ withRoles [Admin] $ Api.update Table.anns
    delete "/announcements/:_id" $ withRoles [Admin] $ Api.deactivate Table.anns
    {-
     - Requests
     -}
    -- TODO Only author and operator can view & edit requests
    get "/requests" $ withAuth $ Api.getMany Table.requests
    get "/requests/:_id" $ withAuth Request.getWithProps
    post "/requests" $ withRoles [Client] Request.createWithProps
    put "/requests/:_id" $ withRoles [Client, Operator] Request.updateWithProps
    {-
     - Teams
     -}
    get "/teams" $ withRoles [Admin] $ Api.getMany Table.teams
    get "/teams/:_id" $ withRoles [Admin] $ Api.get Table.teams
    post "/teams" $ withRoles [Admin] $ Api.create @TeamWithoutId Table.teams
    put "/teams/:_id" $ withRoles [Admin] $ Api.update Table.teams
    delete "/teams/:_id" $ withRoles [Admin] $ Api.deactivate Table.teams
    {-
     - Standard 404 -- keep this last
     -}
    S.notFound $ S.text "Not found"
  where
    withDB = withDBEnv config
    withAuth = withAuthEnv config
    withRoles = withRolesEnv config
