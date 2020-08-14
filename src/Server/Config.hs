{-# LANGUAGE TemplateHaskell #-}

module Server.Config where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Ini
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import System.Environment

data ListenConfig
  = ListenOnPort {_listenOnPort :: Int}
  | ListenOnSocket {_listenOnSocket :: Text}
  deriving (Show)

data Config = Config
  { _dataDir :: Text,
    _dbConn :: Text,
    _listen :: ListenConfig,
    _allowCORS :: Bool,
    _regTokenSecret :: Text
  }
  deriving (Show)

makeLenses ''ListenConfig
makeLenses ''Config

defaultConfig :: Config
defaultConfig =
  Config
    { _dataDir = "data",
      _dbConn = "",
      _listen = ListenOnPort 9080,
      _allowCORS = True, --TODO switch to False later
      _regTokenSecret = "31337" --TODO generate a random token for a single run
    }

dataDirStr :: Config -> String
dataDirStr = unpack . _dataDir

dbConnStr :: Config -> ByteString
dbConnStr = encodeUtf8 . _dbConn

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
    $ upd "data_dir" dataDir id
      . upd "listen_port" listen (ListenOnPort . read . unpack)
      . upd "listen_socket" listen ListenOnSocket
      . upd "db_conn" dbConn id
      . upd "allow_cors" allowCORS (read . unpack)
      . upd "reg_token_secret" regTokenSecret id
    $ defaultConfig
