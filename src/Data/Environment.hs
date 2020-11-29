{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Environment where

import Control.Lens (Getting, (^?))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (get)
import qualified Control.Monad.Trans.Class as TR
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson.Lens (key, _Integral, _String)
import Data.Monoid (First)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import qualified Data.Text.Lazy as Lazy
import Data.UserInfo
import Database.Selda.Backend.Internal
import Database.Selda.PostgreSQL (PG)
import Database.Selda.Frontend (exec)
import Network.HTTP.Types
import Server.Config
import Web.Scotty ()
import Web.Scotty.Trans hiding (finish, get, header, json, jsonData, param, raise, rescue, status, text)
import qualified Web.Scotty.Trans as S


data Env = Env
    { envConfig :: Config,
      envUser :: Maybe UserInfo
    }


type Action a = ActionT Lazy.Text (SeldaM PG) a


newtype EnvAction a = EA {unEA :: ReaderT Env (ActionT Lazy.Text (SeldaM PG)) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)


instance MonadSelda EnvAction where
    type Backend EnvAction = PG
    withConnection m = EA $ lift (lift (S get)) >>= unEA . m


runEnvAction :: EnvAction a -> Env -> Action a
runEnvAction = runReaderT . unEA


-- reexport
lift :: (TR.MonadTrans t, Monad m) => m a -> t m a
lift = TR.lift


jsonData :: FromJSON a => EnvAction a
jsonData = EA $ lift S.jsonData


json :: ToJSON a => a -> EnvAction ()
json = EA . lift . S.json


status :: Status -> EnvAction ()
status = EA . lift . S.status


param :: Parsable a => Text -> EnvAction a
param = EA . lift . S.param . fromStrict


envIO :: IO a -> EnvAction a
envIO = EA . lift . liftAndCatchIO


finish :: EnvAction a
finish = EA . lift $ S.finish


askConfig :: EnvAction Config
askConfig = envConfig <$> ask


text :: Text -> EnvAction ()
text = EA . lift . S.text . fromStrict


files :: EnvAction [File]
files = EA . lift $ S.files


header :: Text -> EnvAction (Maybe Text)
header = EA . lift . fmap (toStrict <$>) . S.header . fromStrict


redirect :: Text -> EnvAction ()
redirect = EA . lift . S.redirect . fromStrict

raise :: Text -> EnvAction a
raise = EA . lift . S.raise . fromStrict

raise_ :: Text -> EnvAction ()
raise_ = void . raise

rescue :: EnvAction a -> (Text -> EnvAction a) -> EnvAction a
rescue act err = do
    env <- ask
    let action = runEnvAction act env
    let catchtion msg = runEnvAction (err $ toStrict msg) env
    EA . lift $ action `S.rescue` catchtion


{- | Perform the given computation atomically.
 -
 - Ripped off from selda, with the explicit MonadMask requirement removed.
 -
 - WARNING: do not call the scotty's unmaskable `finish` from inside
 - transaction; your data won't be committed that way! -}
transaction :: EnvAction a -> EnvAction a
transaction m = transact $ do
  void $ exec "BEGIN TRANSACTION" []
  x <- m `rescue` \err -> do void $ exec "ROLLBACK" []
                             raise err
  void $ exec "COMMIT" []
  return x

askUserInfo :: EnvAction UserInfo
askUserInfo = do
    env <- ask
    case envUser env of
        Just ui -> return ui
        Nothing -> status forbidden403 >> finish


jsonParam :: Text -> Getting (First a) Value a -> EnvAction a
jsonParam s l = do
    js <-
        jsonData `rescue` (catch . (<>) "Query JSON parsing error: ") :: EnvAction Value
    case js ^? l of
        Nothing -> catch ("Missing or malformed parameter: " <> s)
        Just d -> return d
  where
    catch msg = text msg >> status badRequest400 >> finish


jsonParamText :: Text -> EnvAction Text
jsonParamText a = jsonParam a (key a . _String)


jsonParamInt :: Integral a => Text -> EnvAction a
jsonParamInt a = jsonParam a (key a . _Integral)
