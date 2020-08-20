{-# LANGUAGE CPP #-}

module Version where

import Options.Applicative

theVersion :: String
theVersion = VERSION_request2

versionOption :: String -> Parser (a->a)
versionOption prog =
  infoOption
    ( prog
        <> " version "
        <> theVersion
        <> "\nCopyright (C) 2020 IOCB Prague\nThis is free software, you are free to change\nand redistribute it under the terms of GPLv3.\nThere is NO WARRANTY, to the extent permitted by law.\n\nWritten by Evzen Wybitul <evzen.wybitul@uochb.cas.cz>"
    )
    (short 'v' <> long "version" <> help "display version information")

helperOption :: Parser (a -> a)
helperOption =
  abortOption
    (ShowHelpText Nothing)
    (long "help" <> short 'h' <> help "Show this help text")
