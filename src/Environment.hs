module Environment where

import Config
import qualified Control.Monad.Trans.Class as TR
import Control.Monad.Trans.Reader
import Data.Text
import Data.Text.Lazy (fromStrict, toStrict)
import Database.BasicAuth
import qualified Database.SQLite.Simple as DB
import HTTPHelpers
import Model.User (Role)
import UserInfo
import Utils (stringParam)
import Web.Scotty

data Env = Env
  { envConfig :: ServerConfig
  , envDBConn :: Maybe DB.Connection
  , envUser :: Maybe UserInfo
  }

type EnvAction a = ReaderT Env ActionM a

-- reexport
lift :: (TR.MonadTrans t, Monad m) => m a -> t m a
lift = TR.lift {- TODO: many Scotty operations (status, json, text) could have
                  lifted versions just for EnvAction to make life+imports easier -}

envIO :: IO a -> EnvAction a
envIO = lift . liftAndCatchIO

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

envRescue :: EnvAction a -> (Text -> EnvAction a) -> EnvAction a
envRescue act err = do
  env <- ask
  lift $ runReaderT act env `rescue` \msg -> runReaderT (err $ toStrict msg) env

actionThenCloseDB :: EnvAction a -> EnvAction a
actionThenCloseDB ea =
  (ea <* envCloseDB) `envRescue` \msg ->
    envCloseDB >> lift (raise $ fromStrict msg)

withEnv :: ServerConfig -> EnvAction a -> ActionM a
withEnv config ea =
  runReaderT (actionThenCloseDB ea) $ Env config Nothing Nothing

withDBEnv :: ServerConfig -> EnvAction a -> ActionM a
withDBEnv config ea = do
  conn <-
    liftAndCatchIO (DB.open $ dbPath config) `rescue` \msg -> do
      text $ "Database connection failure: " <> msg
      finishServerError
  runReaderT (actionThenCloseDB ea) $ Env config (Just conn) Nothing

withAuthEnv :: ServerConfig -> EnvAction a -> ActionM a
withAuthEnv config = withDBEnv config . authentized

authentized :: EnvAction a -> EnvAction a
authentized action = do
  apikey <- lift $ stringParam "api_key"
  conn <- askDB
  auth <- lift $ liftAndCatchIO $ findApiKeyUser conn apikey
  case auth of
    Just u -> local (\env -> env {envUser = Just u}) action
    _ -> envForbidden

askUser :: EnvAction UserInfo
askUser = do
  mu <- envUser <$> ask
  case mu of
    Just u -> return u
    _ -> lift finishForbidden

askDB :: EnvAction DB.Connection
askDB = do
  mdb <- envDBConn <$> ask
  case mdb of
    Just db -> return db
    _ -> lift $ text "db initialization failure" >> finishBadRequest

askConfig :: EnvAction ServerConfig
askConfig = envConfig <$> ask

checkRoles :: ([Role] -> Bool) -> EnvAction ()
checkRoles cond = do
  ok <- cond . roles <$> askUser
  if ok
    then return ()
    else lift finishForbidden

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
