{-# LANGUAGE TemplateHaskell #-}

module Server.Config where

import Control.Lens
import Data.Ini
import Data.Text


data ListenConfig
    = ListenOnPort {_listenOnPort :: Int}
    | ListenOnSocket {_listenOnSocket :: String}
    deriving (Show)


data Config = Config
    { _dataDir :: String,
      _dataUrlPrefix :: String,
      _dbUser :: Text, --TODO eventually modify selda to pass in a connection string, as with internal pgOpen'
      _dbHost :: Text,
      _listen :: ListenConfig,
      _allowCORS :: Bool,
      _regTokenSecret :: String,
      _mailEnvelopeFrom :: Text,
      _mailFrom :: Text,
      _mailFromName :: Text,
      _mailReplyTo :: Text,
      _frontendUrlBase :: Text
    }
    deriving (Show)


makeLenses ''ListenConfig
makeLenses ''Config


defaultConfig :: Config
defaultConfig =
    Config
        { _dataDir = "data",
          _dataUrlPrefix = "/data",
          _dbUser = "request",
          _dbHost = "localhost",
          _listen = ListenOnPort 9080,
          _allowCORS = False,
          _regTokenSecret = "31337", --TODO eventually generate a random token for a single run
          _mailEnvelopeFrom = "request@request.cz",
          _mailFrom = "noreply@request.cz",
          _mailFromName = "Request",
          _mailReplyTo = "request@request.cz",
          _frontendUrlBase = "http://localhost:3000"
        }


defaultConfigPath :: String
defaultConfigPath = "etc/default.cfg"


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
    return $
        upd "data_dir" dataDir unpack
            . upd "data_url_prefix" dataUrlPrefix unpack
            . upd "listen_port" listen (ListenOnPort . read . unpack)
            . upd "listen_socket" listen (ListenOnSocket . unpack)
            . upd "db_user" dbUser id
            . upd "db_host" dbHost id
            . upd "allow_cors" allowCORS (read . unpack)
            . upd "reg_token_secret" regTokenSecret unpack
            . upd "mail_envelope_from" mailEnvelopeFrom id
            . upd "mail_from" mailFrom id
            . upd "mail_from_name" mailFromName id
            . upd "mail_reply_to" mailReplyTo id
            . upd "frontend_url_base" frontendUrlBase id
            $ defaultConfig
