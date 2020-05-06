{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Ini
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import System.Environment

data ServerConfig
  = ServerConfig
      { _dataDir :: Text,
        _dbPath :: Text,
        _listenPort :: Int,
        _allowCORS :: Bool,
        _regTokenSecret :: Text
      }
  deriving (Show)

makeLenses ''ServerConfig

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { _dataDir = "data",
      _dbPath = "data/database.postgresql",
      _listenPort = 9080,
      _allowCORS = True, --TODO switch to False later
      _regTokenSecret = "31337" --TODO generate a random token for a single run
    }

dataDirStr :: ServerConfig -> String
dataDirStr = unpack . _dataDir

dbPathStr :: ServerConfig -> ByteString
dbPathStr = encodeUtf8 . _dbPath

defaultConfigPath :: String
defaultConfigPath = "etc/default.cfg"

getConfig :: IO ServerConfig
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
  ASetter' ServerConfig Text ->
  (ServerConfig -> ServerConfig)
updateFromIni ini sec name l =
  either (const id) (set l) $ lookupValue sec name ini

asText :: (Show a, Read a) => Iso' a Text
asText = iso (pack . show) (read . unpack) --not really an iso but whatever

readConfig :: String -> IO ServerConfig
readConfig path = do
  cfg <- readIniFile path
  ini <-
    case cfg of
      Left err -> error $ "Could not read config: " ++ err
      Right a -> pure a
  let upd = updateFromIni ini "server"
  return
    $ upd "data_dir" dataDir
      . upd "listen_port" (listenPort . asText)
      . upd "db_path" dbPath
      . upd "allow_cors" (allowCORS . asText)
      . upd "reg_token_secret" regTokenSecret
    $ defaultConfig
