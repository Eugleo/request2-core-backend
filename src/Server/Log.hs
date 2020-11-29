module Server.Log where

import Data.Environment
import Foreign.C.String (withCStringLen)
import qualified System.Posix.Syslog as S


syslog' :: S.Priority -> String -> IO ()
syslog' prio s = withCStringLen s (S.syslog Nothing prio)


syslog :: S.Priority -> String -> EnvAction ()
syslog prio s = envIO (syslog' prio s)


withLog :: IO a -> IO a
withLog = S.withSyslog "request2" [S.LogPID, S.Console] S.User


logOperation' :: String -> IO ()
logOperation' = syslog' S.Info


logError' :: String -> IO ()
logError' = syslog' S.Error


logOperation :: String -> EnvAction ()
logOperation = syslog S.Info


logUserAuth :: String -> EnvAction ()
logUserAuth = syslog S.Notice


logAuthFail :: String -> EnvAction ()
logAuthFail = syslog S.Warning


logError :: String -> EnvAction ()
logError = syslog S.Error


logFatal :: String -> EnvAction ()
logFatal = syslog S.Alert
