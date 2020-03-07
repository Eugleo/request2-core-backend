module Crypto where

import Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString.Lazy.UTF8 as LU (fromString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Entropy (getEntropy)
import System.POSIX.Crypt.SHA512

-- hash-related stuff
newHash password =
  toString . cryptSHA512' (Just 31337) (fromString password) <$> getEntropy 128

checkHash password hashStr = Just hash == cryptSHA512 (fromString password) hash
  where
    hash = fromString hashStr

-- notice: make sure your system's prng is not dim-witted
newApiKey = toString . encode64 <$> getEntropy 32

pathHash = showDigest . sha256 . LU.fromString
pathHash' = showDigest . sha256
