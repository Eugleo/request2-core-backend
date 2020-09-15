{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Files where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (toString)
import Data.Environment
  ( EnvAction,
    askConfig,
    envIO,
    files,
    json,
    param,
    raise,
    redirect,
    rescue,
    status,
  )
import Data.List (intercalate)
import Data.Maybe
import Data.Model.Property (Property)
import Data.Model.PropertyType (PropertyType (..))
import qualified Data.Text as T
import qualified Database.Selda as S
import Database.Table (properties)
import Network.HTTP.Types.Status (created201, notFound404, notModified304)
import Network.Wai.Parse (FileInfo (..))
import Server.Config (_dataDir, _dataUrlPrefix)
import System.Directory (createDirectoryIfMissing)
import Utils.Crypto (getRandomHash)

upload :: EnvAction ()
upload = do
  maybeFiles <- handleUploads
  case catMaybes maybeFiles of
    [] -> status notModified304
    fs -> do
      let fileObjects = map (\(h, m, n) -> object ["hash" .= h, "mime" .= m, "name" .= n]) fs
      json $ object ["files" .= fileObjects]
      status created201

fileDir' :: String -> T.Text -> String
fileDir' pfx hash =
  intercalate "/" . (pfx :) $
    map
      T.unpack
      [ T.take 2 hash,
        T.take 2 $ T.drop 2 hash,
        T.take 2 $ T.drop 4 hash,
        T.drop 6 hash
      ]

fileDir :: T.Text -> EnvAction String
fileDir hash = do
  pfx <- _dataDir <$> askConfig
  return $ fileDir' pfx hash

filePath' :: String -> T.Text -> T.Text -> String
filePath' pfx hash name = fileDir' pfx hash ++ "/" ++ T.unpack name

filePath :: T.Text -> T.Text -> EnvAction String
filePath h n = do
  pfx <- _dataDir <$> askConfig
  return $ filePath' pfx h n

fileUrl :: T.Text -> T.Text -> EnvAction String
fileUrl h n = do
  pfx <- _dataUrlPrefix <$> askConfig
  return $ filePath' pfx h n

propIsFileWithHash :: S.Text -> S.Row t Property -> S.Col t Bool
propIsFileWithHash h property =
  ((property S.! #propertyData) `S.like` S.literal (T.append h ":%"))
    S..&& ( (property S.! #propertyType S..== S.literal File)
              S..|| (property S.! #propertyType S..== S.literal ResultFile)
          )

getUniqueHash :: EnvAction T.Text
getUniqueHash = do
  hash <- envIO getRandomHash
  -- TODO this needs an index!!!
  res <-
    S.query $ S.select properties `S.suchThat` propIsFileWithHash hash
  case res of
    [] -> return hash
    _ -> getUniqueHash

fileDescToProperty :: (T.Text, T.Text, T.Text) -> T.Text
fileDescToProperty (h, m, n) = T.concat [h, ":", m, ":", n]

filePropertyToDesc :: T.Text -> Maybe (T.Text, T.Text, T.Text)
filePropertyToDesc p = do
  let (h, p1) = T.break (== ':') p
  (_, p2) <- T.uncons p1
  let (m, p3) = T.break (== ':') p2
  (_, n) <- T.uncons p3
  return (h, m, n)

handleUploads :: EnvAction [Maybe (T.Text, T.Text, T.Text)]
handleUploads = files >>= traverse handleUpload'
  where
    handleUpload' (_, x) = (Just <$> handleUpload x) `rescue` const (return Nothing)

    handleUpload :: FileInfo BL.ByteString -> EnvAction (T.Text, T.Text, T.Text)
    handleUpload
      ( FileInfo
          { fileName = name',
            fileContentType = mime',
            fileContent = d
          }
        ) = do
        hash <- getUniqueHash --TODO check if it is unique
        let mime = T.pack $ toString mime'
            name = T.pack $ toString name'
        path <- filePath hash name
        dir <- fileDir hash
        envIO $ createDirectoryIfMissing True dir
        envIO $ B.writeFile path $ BL.toStrict d
        return $ (hash, mime, name)

success :: ToJSON a => a -> EnvAction ()
success v = json (object ["data" .= toJSON v])

getFile :: EnvAction ()
getFile = do
  hash <- param "hash"
  --TODO this needs more checking (ie "is it really a file")
  --TODO share the condition
  res <-
    S.query $ do
      prop <- S.select properties `S.suchThat` propIsFileWithHash hash
      return $ prop S.! #propertyData
  envIO $ print res
  case res of
    [d] -> do
      case filePropertyToDesc d of
        Just (h, _, n) -> fileUrl h n >>= success
        _ -> raise "File property decoding problem"
    _ -> status notFound404
