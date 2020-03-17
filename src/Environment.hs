module Environment where

import Config
import Control.Monad (unless)
import qualified Control.Monad.Trans.Class as TR
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Map ((!?))
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.BasicAuth
import qualified Database.SQLite.Simple as DB
import HTTPHelpers
import Model.User (Role)
import Network.HTTP.Types (Status)
import UserInfo
import Web.Scotty hiding (json, jsonData, param, rescue, status, text)
import qualified Web.Scotty as S
  ( json,
    jsonData,
    param,
    rescue,
    status,
    text,
  )

data Env
  = Env
      { envConfig :: ServerConfig,
        envDBConn :: Maybe DB.Connection,
        envUser :: Maybe UserInfo
      }

type EnvAction a = ReaderT Env ActionM a

-- reexport
lift :: (TR.MonadTrans t, Monad m) => m a -> t m a
lift = TR.lift

jsonData :: FromJSON a => EnvAction a
jsonData = lift S.jsonData

json :: ToJSON a => a -> EnvAction ()
json = lift . S.json

status :: Status -> EnvAction ()
status = lift . S.status

param :: Parsable a => Text -> EnvAction a
param = lift . S.param . fromStrict

envIO :: IO a -> EnvAction a
envIO = lift . liftAndCatchIO

rescue :: EnvAction a -> (Text -> EnvAction a) -> EnvAction a
rescue act err = do
  env <- ask
  let action = runReaderT act env
  let catchtion msg = runReaderT (err $ toStrict msg) env
  lift $ action `S.rescue` catchtion

text :: Text -> EnvAction ()
text = lift . S.text . fromStrict

jsonParam :: Text -> EnvAction Text
jsonParam s = do
  js <- jsonData `rescue` (catch . (<>) "Query JSON parsing error: ")
  case js !? s of
    Nothing -> catch ("Missing string parameter: " <> s)
    Just d -> return d
  where
    catch msg = do
      text msg
      envBadRequest

{- sadly, ActionM doesn't have MonadMask and cannot be easily bracketed. We
 - therefore use the following strategy:
 - 1. `finish` actions are wrapped with envFinish (and convenience wrappers)
 - 2. db is proactively closed after EnvActionM finishes by default (e.g. by
 -    `withDBEnv`)
 - 3. we hope that no one will use normal `finish` as it leaves the open file
 -    descriptor hanging for the GC. -}
envCloseDB :: EnvAction ()
envCloseDB = do
  mdb <- envDBConn <$> ask
  case mdb of
    Just db -> envIO $ DB.close db
    _ -> return ()

actionThenCloseDB :: EnvAction a -> EnvAction a
actionThenCloseDB ea =
  (ea <* envCloseDB) `rescue` \msg ->
    envCloseDB >> lift (raise $ fromStrict msg)

withEnv :: ServerConfig -> EnvAction a -> ActionM a
withEnv config ea =
  runReaderT (actionThenCloseDB ea) $ Env config Nothing Nothing

withDBEnv :: ServerConfig -> EnvAction a -> ActionM a
withDBEnv config ea = do
  conn <-
    liftAndCatchIO (DB.open $ dbPath config) `S.rescue` \msg -> do
      S.text $ "Database connection failure: " <> msg
      finishServerError
  runReaderT (actionThenCloseDB ea) $ Env config (Just conn) Nothing

withAuthEnv :: ServerConfig -> EnvAction a -> ActionM a
withAuthEnv config = withDBEnv config . authentized

authentized :: EnvAction a -> EnvAction a
authentized action = do
  apikey <- jsonParam "api_key"
  conn <- askDB
  auth <- envIO $ findApiKeyUser conn apikey
  case auth of
    Just u -> local (\env -> env {envUser = Just u}) action
    _ -> envForbidden

withRolesEnv :: ServerConfig -> [Role] -> EnvAction a -> ActionM a
withRolesEnv config rs action =
  withAuthEnv config $ do
    userRoles <- roles <$> askUser
    unless (all (`elem` rs) userRoles) envForbidden
    action

askUser :: EnvAction UserInfo
askUser = do
  mu <- envUser <$> ask
  case mu of
    Just u -> return u
    _ -> envForbidden

askDB :: EnvAction DB.Connection
askDB = do
  mdb <- envDBConn <$> ask
  case mdb of
    Just db -> return db
    _ -> text "db initialization failure" >> envBadRequest

askConfig :: EnvAction ServerConfig
askConfig = envConfig <$> ask

envFinish :: EnvAction a
envFinish = envCloseDB >> lift finish

envForbidden :: EnvAction a
envForbidden = envCloseDB >> lift finishForbidden

envBadRequest :: EnvAction a
envBadRequest = envCloseDB >> lift finishBadRequest

envNotFound :: EnvAction a
envNotFound = envCloseDB >> lift finishNotFound

envServerError :: EnvAction a
envServerError = envCloseDB >> lift finishServerError