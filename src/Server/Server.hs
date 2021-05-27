{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Server.Server where

import qualified Api.Common as Api
import qualified Api.Files as Files
import Api.Query.Announcement (annQueryTranslator)
import Api.Query.Request (requestQueryTranslator)
import Api.Query.Runner
import Api.Query.Team
import qualified Api.Request as Request
import qualified Api.User as User
import Control.Exception (bracket)
import Control.Monad
import Data.AnnWithoutId (AnnWithoutId)
import Data.Bool (bool)
import Data.Default.Class (def)
import Data.Model.Role (Role (..))
import Data.TeamWithoutId (TeamWithoutId)
import qualified Data.Text as T
import Database.Selda (SeldaT)
import Database.Selda.PostgreSQL (PG, PGConnectInfo (..), withPostgreSQL)
import qualified Database.Table as Table
import qualified Network.Socket as NS
import Network.Wai
import Server.Capability
import Server.Config
import System.Directory (doesFileExist, removeFile)
import Utils.Feedback
import Utils.WithX
import Web.Scotty.Trans (delete, get, post, put, scottySocketT, scottyT)
import qualified Web.Scotty.Trans as S (
    ScottyT,
    function,
    json,
    middleware,
    notFound,
    options,
    text,
 )


connInfo :: Config -> PGConnectInfo
connInfo =
    PGConnectInfo
        <$> _dbHost <*> pure 5432 <*> _dbName <*> _dbSchema <*> _dbUser <*> _dbPassword


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
    post "/users" $ withRoles [Admin] User.createNew
    get "/users" $ withRoles [Admin] User.getUsers
    put "/users/:_id" $ withRoles [Admin] User.updateUser
    delete "/users/:_id" $ withRoles [Admin] $ Api.deactivate Table.users
    {-
     - Capabilities
     -}
    get "/capability" $ S.json (capabilityList :: [T.Text])
    post "/feedback" $ withAuth sendFeedback
    {-
     - Users
     -}
    get "/users/:_id" $ withRoles [Admin, Operator] User.getUser
    post "/register-init" $ withDB User.sendRegToken
    post "/register" $ withDB User.register
    post "/login" $ withDB User.login
    post "/logout" $ withAuth User.logout
    post "/change-password" $ withAuth User.changePassword
    post "/password-reset" $ withDB User.resetPassword
    post "/password-reset-init" $ withDB User.sendPwdResetEmail
    {-
     - User information
     -}
    get "/me" $ withAuth User.getDetails
    get "/users/:_id/name" $ withAuth User.getName
    put "/me" $ withAuth User.editMe
    {-
     - Announcements
     -}
    get "/announcements" $ withAuth $ void $ runQuery Table.anns annQueryTranslator
    get "/announcements/:_id" $ withAuth $ Api.get Table.anns
    post "/announcements" $ withRoles [Admin] $ Api.create @AnnWithoutId Table.anns
    put "/announcements/:_id" $ withRoles [Admin] $ Api.update Table.anns
    delete "/announcements/:_id" $ withRoles [Admin] $ Api.deactivate Table.anns
    {-
     - Requests
     -}
    get "/me/requests" $ withAuth Request.getMyRequests -- DONE
    get "/requests" $
        void $
            withRoles [Operator, Admin] $
                runQuery Table.requests requestQueryTranslator -- DONE
    get "/requests/:_id" $ withAuth Request.getRequest -- DONE
    get "/requests/:_id/props" $ withAuth Request.getProperties -- DONE
    get "/requests/:_id/comments" $ withAuth Request.getComments -- DONE
    post "/requests" $ withRoles [Client] Request.createWithProps -- DONE
    put "/requests/:_id/status" $ withRoles [Operator, Admin] Request.updateStatus -- DONE
    put "/requests/:_id" $ withRoles [Client, Operator, Admin] Request.updateWithProps -- DONE
    put "/requests/:_id/results" $ withRoles [Operator, Admin] Request.updateResults -- DONE
    post "/requests/:_id/comments" $ withRoles [Client, Operator] Request.addComment -- DONE
    {-
     - Files
     -}
    -- TODO is auth needed here?
    post "/files" $ withDB Files.upload
    delete "/files/:hash" $ withDB Files.delete
    get "/files/:hash" $ withDB Files.getFile
    {-
     - Teams
     -}
    get "/teams" $ withRoles [Admin, Operator] $ void $ runQuery Table.teams teamQueryTranslator
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
