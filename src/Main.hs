module Main where

import Control.Monad
import Database.Selda.PostgreSQL
import qualified Database.Table as DB
import Options.Applicative
import Server.Config
import Server.Server
import Server.Log
import Version


data Opts = Opts
    { optAction :: Action,
      optConfig :: String
    }
    deriving (Show)


data Action
    = RunServer
    | SetupDatabase
    deriving (Show)


oOpts :: Parser Opts
oOpts =
    Opts <$> oAction
        <*> strOption
            (metavar "CONFIG" <> short 'c' <> long "config" <> value defaultConfigPath)


oAction :: Parser Action
oAction =
    subparser $
        mconcat
            [ command "run-server" $ info (pure RunServer) $ progDesc "Start the server",
              command "create-db" $
                info (pure SetupDatabase) $ progDesc "Populate the database tables"
            ]


main :: IO ()
main = withLog $
    let opts :: ParserInfo Opts
        opts =
            info
                (oOpts <**> versionOption "request2" <**> helperOption)
                ( fullDesc
                    <> progDesc
                        "request2 is a Scotty-based API server for a request tracking system developed at IOCB."
                    <> header "request2: request systam API"
                )
     in do
            o <- execParser opts
            cfg <- readConfig $ optConfig o
            case optAction o of
                RunServer -> do
                  logOperation' "Starting the server"
                  void $ server cfg
                SetupDatabase -> do
                  logOperation' "Database setup"
                  withPostgreSQL (connInfo cfg) DB.createAll
