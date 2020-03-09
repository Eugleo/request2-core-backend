module Crypto where

import Data.ByteString.Lazy.UTF8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as LU (fromString)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Entropy (getEntropy)
import System.POSIX.Crypt.SHA512

type Hash = String

newHash :: String -> IO Hash
newHash password =
  toString . cryptSHA512' (Just 31337) (fromString password) <$> getEntropy 128

checkHash :: String -> Hash -> Bool
checkHash password hashStr = Just hash == cryptSHA512 (fromString password) hash
  where
    hash = fromString hashStr

-- TODO make sure your system's prng is not dim-witted
newApiKey :: IO String
newApiKey = toString . encode64 <$> getEntropy 32

pathHash :: String -> String
pathHash = showDigest . sha256 . LU.fromString

pathHash' :: ByteString -> String
pathHash' = showDigest . sha256
