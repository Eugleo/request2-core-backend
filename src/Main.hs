{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.String
import Database.Selda.PostgreSQL
import qualified Database.Table as DB
import Options.Applicative
import Server.Config
import Server.Log
import Server.Server
import Utils.Mail.Common
import Version


data Opts = Opts
    { optAction :: Action,
      optConfig :: String
    }
    deriving (Show)


data Action
    = RunServer
    | SetupDatabase
    | MailTest String
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
                info (pure SetupDatabase) $ progDesc "Populate the database tables",
              command "mail-test" $
                info (MailTest <$> strArgument (metavar "EMAIL")) $
                    progDesc "Send a testing debug e-mail to a specified address"
            ]


main :: IO ()
main =
    withLog $
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
                    MailTest eml -> do
                        logOperation' $ "Sending a test mail to " ++ eml
                        sendmail' cfg $
                            textMail'
                                cfg
                                (Address (Just "Test mail recipient") (fromString eml))
                                "Request 2 test e-mail"
                                "Hello,\n\nif you read this, the e-mail sending pipeline apparently works.\n\nBest,\nRequest2"
