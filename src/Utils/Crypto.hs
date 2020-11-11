module Utils.Crypto where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.UTF8 as LU
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha256, sha512, showDigest)
import Data.Text
import Server.Config
import System.Entropy (getEntropy)
import System.POSIX.Crypt.SHA512
import Text.Email.Validate (isValid)


type ApiKey = Text


type Hash = Text


newHash :: Text -> IO Hash
newHash password =
    pack . toString . cryptSHA512' (Just 31337) (fromString $ unpack password)
        <$> getEntropy 128


checkHash :: Text -> Hash -> Bool
checkHash password hashStr =
    Just hash == cryptSHA512 (fromString $ unpack password) hash
  where
    hash = fromString $ unpack hashStr


-- TODO make sure your system's prng is not dim-witted
newApiKey :: IO (ApiKey, Hash)
newApiKey = do
    userKey <- pack . toString . B64.encode <$> getEntropy 32
    return (userKey, dbApiKey userKey)


dbApiKey :: ApiKey -> Hash
dbApiKey =
    pack
        . showDigest
        . sha256
        . fromStrict
        . B64.decodeLenient
        . fromString
        . unpack


getRandomHash :: IO Text
getRandomHash = pack . toString . B16.encode <$> getEntropy 16 --overkill.


regToken :: Text -> Config -> Maybe Hash
regToken mail config
    | isValid (toStrict bmail) = Just token
    | otherwise = Nothing
  where
    bs = LU.fromString
    bmail = bs $ unpack mail
    padding = bs $ _regTokenSecret config
    token =
        pack . showDigest . sha512 $ padding <> "\0" <> bmail <> "\0" <> padding
