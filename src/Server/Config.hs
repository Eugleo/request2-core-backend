{-# LANGUAGE TemplateHaskell #-}

module Server.Config where

import Control.Lens
import Data.Ini
import Data.Text
import System.Environment

data ListenConfig
  = ListenOnPort {_listenOnPort :: Int}
  | ListenOnSocket {_listenOnSocket :: String}
  deriving (Show)

data Config = Config
  { _dataDir :: String,
    _dbUser :: Text, --TODO eventually modify selda to pass in a connection string, as with internal pgOpen'
    _dbHost :: Text,
    _listen :: ListenConfig,
    _allowCORS :: Bool,
    _regTokenSecret :: String
  }
  deriving (Show)

makeLenses ''ListenConfig
makeLenses ''Config

defaultConfig :: Config
defaultConfig =
  Config
    { _dataDir = "data",
      _dbUser = "request",
      _dbHost = "localhost",
      _listen = ListenOnPort 9080,
      _allowCORS = False,
      _regTokenSecret = "31337" --TODO eventually generate a random token for a single run
    }

defaultConfigPath :: String
defaultConfigPath = "etc/default.cfg"

getConfig :: IO Config
getConfig = do
  args <- getArgs
  case args of
    [] -> readConfig defaultConfigPath
    [cf] -> readConfig cf
    _ -> error "Specify at most one parameter with config filename"

updateFromIni ::
  Ini ->
  Text ->
  Text ->
  ASetter' Config a ->
  (Text -> a) ->
  (Config -> Config)
updateFromIni ini sec name l f =
  either (const id) (set l . f) $ lookupValue sec name ini

readConfig :: String -> IO Config
readConfig path = do
  cfg <- readIniFile path
  ini <-
    case cfg of
      Left err -> error $ "Could not read config: " ++ err
      Right a -> pure a
  let upd = updateFromIni ini "server"
  return
    $ upd "data_dir" dataDir unpack
      . upd "listen_port" listen (ListenOnPort . read . unpack)
      . upd "listen_socket" listen (ListenOnSocket . unpack)
      . upd "db_user" dbUser id
      . upd "db_host" dbHost id
      . upd "allow_cors" allowCORS (read . unpack)
      . upd "reg_token_secret" regTokenSecret unpack
    $ defaultConfig
