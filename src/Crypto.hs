module Crypto where

import Data.ByteString.Lazy.UTF8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as LU (fromString)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.Text
import System.Entropy (getEntropy)
import System.POSIX.Crypt.SHA512

type Hash = Text

newHash :: Text -> IO Hash
newHash password =
  pack . toString . cryptSHA512' (Just 31337) (fromString $ unpack password)
    <$> getEntropy 128

checkHash :: Text -> Hash -> Bool
checkHash password hashStr = Just hash == cryptSHA512 (fromString $ unpack password) hash
  where
    hash = fromString $ unpack hashStr

-- TODO make sure your system's prng is not dim-witted
newApiKey :: IO Text
newApiKey = pack . toString . encode64 <$> getEntropy 32

pathHash :: Text -> Hash
pathHash = pack . showDigest . sha256 . LU.fromString . unpack

pathHash' :: ByteString -> Hash
pathHash' = pack . showDigest . sha256
