module Database.General (withConfig) where

import Config
import Database.SQLite.Simple

withConfig :: ServerConfig -> (Connection -> IO a) -> IO a
withConfig = withConnection . dbPath
