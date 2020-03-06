module Config where

import Data.Ini
import Data.Text
import System.Environment

data ServerConfig = ServerConfig
  { dataDir :: String
  , listenPort :: Int
  } deriving (Show)

defaultConfig =
  ServerConfig
    { dataDir = "/var/lib/request2"
    , listenPort = 9080
    }

defaultConfigFile = "/etc/request2.cfg"

getConfig = do
  args <- getArgs
  case args of
    [] -> readConfig defaultConfigFile
    [cf] -> readConfig cf
    _ -> error "specify at most one parameter with config filename"

updateFromIni ::
     Ini
  -> String
  -> String
  -> (ServerConfig -> String -> ServerConfig)
  -> ServerConfig
  -> ServerConfig
updateFromIni ini sec name f c =
  either (const c) (f c) $ unpack <$> lookupValue (pack sec) (pack name) ini

readConfig :: String -> IO ServerConfig
readConfig fn = do
  cfg <- readIniFile fn
  ini <-
    case cfg of
      Left err -> error $ "could not read config: " ++ err
      Right a -> pure a
  let upd = updateFromIni ini "server"
  return
    (upd "data_dir" (\a b -> a {dataDir = b}) .
     upd "listen_port" (\a b -> a {listenPort = read b}) $
     defaultConfig)

-- use this
dbPath sc = dataDir sc ++ "/db.sqlite"

userMailbox sc user = dataDir sc ++ "/mbox/" ++ user
