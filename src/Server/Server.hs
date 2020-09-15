{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Server.Server where

import qualified Api.Common as Api
import qualified Api.Files as Files
import qualified Api.Request as Request
import qualified Api.User as User
import Control.Exception (bracket)
import Control.Monad
import Data.AnnWithoutId (AnnWithoutId)
import Data.Bool (bool)
import Data.Default.Class (def)
import Data.Model.Role (Role (..))
import Data.PropertyWithoutId (PropertyWithoutId)
import Data.TeamWithoutId (TeamWithoutId)
import qualified Data.Text as T
import Database.Selda (SeldaT)
import Database.Selda.PostgreSQL (PG, PGConnectInfo (..), on, withPostgreSQL)
import qualified Database.Table as Table
import qualified Network.Socket as NS
import Network.Wai
import Server.Capability
import Server.Config
import System.Directory (doesFileExist, removeFile)
import Utils.WithX
import Web.Scotty.Trans (delete, get, post, put, scottySocketT, scottyT)
import qualified Web.Scotty.Trans as S
  ( ScottyT,
    function,
    json,
    middleware,
    notFound,
    options,
    text,
  )

-- TODO Replace with info from config
connInfo :: Config -> PGConnectInfo
connInfo = on <$> _dbUser <*> _dbHost

addCORSHeader :: Middleware
addCORSHeader =
  modifyResponse . mapResponseHeaders $
    (++)
      [ ("Access-Control-Allow-Origin", "*"),
        ("Access-Control-Allow-Headers", "*"),
        ("Access-Control-Allow-Methods", "*")
      ]

withUnixSocket :: String -> (NS.Socket -> IO a) -> IO a
withUnixSocket s = bracket o c
  where
    o = do
      doesFileExist s >>= bool (pure ()) (removeFile s)
      soc <- NS.socket NS.AF_UNIX NS.Stream 0
      NS.bind soc $ NS.SockAddrUnix s
      NS.listen soc $ max 32 NS.maxListenQueue
      return soc
    c soc = do
      NS.close soc
      removeFile s

runScotty :: Config -> S.ScottyT e (SeldaT PG IO) () -> IO ()
runScotty config =
  case _listen config of
    ListenOnPort p -> scottyT p db
    ListenOnSocket s -> \app -> withUnixSocket s (\sock -> scottySocketT def sock db app)
  where
    db = withPostgreSQL (connInfo config)

server :: Config -> IO ()
server config = runScotty config $ do
  when (_allowCORS config) $ do
    S.middleware addCORSHeader
    S.options (S.function $ const $ Just []) $ S.text "CORS OK"
  {-
   - Admin interface
   -}
  post "/users" $ withRoles [Admin] $ User.createNew
  get "/users" $ withRoles [Admin, Operator] $ Api.getMany Table.users
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
  post "/requests/:_id/data/results" $ withDB Files.upload
  get "/requests" $ withAuth $ Api.getMany Table.requests
  get "/requests/:_id" $ withAuth Request.getWithProps
  get "/requests/:_id/comments" $ withRoles [Client, Operator] Request.getComments
  post "/requests" $ withRoles [Client] Request.createWithProps
  put "/requests/:_id" $ withRoles [Client, Operator] Request.updateWithProps
  post "/requests/:_id/comments" $
    withRoles [Client, Operator] $
      Api.create @PropertyWithoutId Table.properties
  {-
   - Files
   -}
  -- TODO is auth needed here?
  get "/file/:hash" $ withDB $ Files.getFile
  {-
   - Teams
   -}
  get "/teams" $ withRoles [Admin, Operator] $ Api.getMany Table.teams
  get "/teams/:_id" $ withRoles [Admin, Operator] $ Api.get Table.teams
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
