module Config where

import Data.Ini
import Data.Text
import System.Environment

data ServerConfig
  = ServerConfig
      { dataDir :: String,
        dbPath :: String,
        listenPort :: Int
      }
  deriving (Show)

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { dataDir = "data",
      dbPath = "data/database.sqlite",
      listenPort = 9080
    }

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
  String ->
  String ->
  (ServerConfig -> String -> ServerConfig) ->
  ServerConfig ->
  ServerConfig
updateFromIni ini sec name f c =
  either (const c) (f c) $ unpack <$> lookupValue (pack sec) (pack name) ini

readConfig :: String -> IO ServerConfig
readConfig path = do
  cfg <- readIniFile path
  ini <-
    case cfg of
      Left err -> error $ "Could not read config: " ++ err
      Right a -> pure a
  let upd = updateFromIni ini "server"
  return
    $ upd "data_dir" (\c new -> c {dataDir = new})
      . upd "listen_port" (\c new -> c {listenPort = read new})
      . upd "db_path" (\c new -> c {dbPath = new})
    $ defaultConfig
