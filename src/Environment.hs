{-# LANGUAGE RankNTypes #-}

module Environment where

import Config
import Control.Lens (Traversal', (^?))
import qualified Control.Monad.Trans.Class as TR
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson.Lens (_Integral, _String, key)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import qualified Data.Text.Lazy as Lazy
import Database.Selda.Backend.Internal (SeldaM)
import Database.Selda.PostgreSQL (PG)
import Network.HTTP.Types
import UserInfo
import qualified Web.Scotty.Trans as S (json, jsonData, param, rescue, status, text)
import Web.Scotty.Trans hiding (json, jsonData, param, rescue, status, text)

data Env
  = Env
      { envConfig :: ServerConfig,
        envUser :: Maybe UserInfo
      }

type Action a = ActionT Lazy.Text (SeldaM PG) a

type EnvAction a = ReaderT Env (ActionT Lazy.Text (SeldaM PG)) a

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

askConfig :: EnvAction ServerConfig
askConfig = envConfig <$> ask

askUserInfo :: EnvAction UserInfo
askUserInfo = do
  env <- ask
  case envUser env of
    Just ui -> return ui
    Nothing -> status forbidden403 >> lift finish

rescue :: EnvAction a -> (Text -> EnvAction a) -> EnvAction a
rescue act err = do
  env <- ask
  let action = runReaderT act env
  let catchtion msg = runReaderT (err $ toStrict msg) env
  lift $ action `S.rescue` catchtion

text :: Text -> EnvAction ()
text = lift . S.text . fromStrict

jsonParam :: Text -> Traversal' Value a -> EnvAction a
jsonParam s l = do
  js <-
    jsonData `rescue` (catch . (<>) "Query JSON parsing error: ") :: EnvAction Value
  case js ^? (key s . l) of
    Nothing -> catch ("Missing or malformed parameter: " <> s)
    Just d -> return d
  where
    catch msg = text msg >> status badRequest400 >> lift finish

jsonParamText :: Text -> EnvAction Text
jsonParamText a = jsonParam a _String

jsonParamInt :: Integral a => Text -> EnvAction a
jsonParamInt a = jsonParam a _Integral
