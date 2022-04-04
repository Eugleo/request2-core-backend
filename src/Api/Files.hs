{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Files where

import Api.Common (success)
import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (toString)
import Data.Environment (
    EnvAction,
    askConfig,
    envIO,
    files,
    json,
    jsonData,
    param,
    raise,
    redirect,
    rescue,
    status,
 )
import qualified Data.FileDesc as F
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Model.Property (Property)
import qualified Data.Text as T
import qualified Database.Selda as S
import Database.Table (properties)
import Network.HTTP.Types.Status (created201, forbidden403, notFound404, notModified304)
import Network.Wai.Parse (FileInfo (..))
import Server.Config (_dataDir, _dataUrlPrefix)
import System.Directory (createDirectoryIfMissing, removeFile)
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


delete :: EnvAction ()
delete = do
    fileDesc <- jsonData
    path <- filePath (F.hash fileDesc) (F.name fileDesc)
    res <-
        S.query $ do
            prop <- S.select properties `S.suchThat` propIsFileWithHash (F.hash fileDesc)
            return $ prop S.! #value
    case res of
        [] -> envIO $ removeFile path
        _ -> status forbidden403


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


fileUrl :: T.Text -> T.Text -> EnvAction T.Text
fileUrl h n = do
    pfx <- _dataUrlPrefix <$> askConfig
    return . T.pack $ filePath' pfx h n


-- TODO this needs an index!!!
propIsFileWithHash :: S.Text -> S.Row t Property -> S.Col t Bool
propIsFileWithHash h property = (property S.! #value) `S.like` S.literal (T.append h ":%")


getUniqueHash :: EnvAction T.Text
getUniqueHash = do
    hash <- envIO getRandomHash
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
        FileInfo
            { fileName = name',
              fileContentType = mime',
              fileContent = d
            } =
            do
                hash <- getUniqueHash --TODO check if it is unique
                let mime = T.pack $ toString mime'
                    name = T.pack $ toString name'
                path <- filePath hash name
                dir <- fileDir hash
                envIO $ createDirectoryIfMissing True dir
                envIO $ B.writeFile path $ BL.toStrict d
                return (hash, mime, name)


findFileUrl :: EnvAction (Maybe T.Text)
findFileUrl = do
    hash <- param "hash"
    res <-
        S.query $ do
            prop <- S.select properties `S.suchThat` propIsFileWithHash hash
            return $ prop S.! #value
    case res of
        [d] ->
            case filePropertyToDesc d of
                Just (h, _, n) -> Just <$> fileUrl h n
                _ -> raise "File property decoding problem" >> return Nothing
        _ -> status notFound404 >> return Nothing


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a


getFileUrl :: EnvAction ()
getFileUrl = do
    maybeUrl <- findFileUrl
    whenJust maybeUrl $ \url ->
        success $ object ["url" .= url]


getFile :: EnvAction ()
getFile = do
    maybeUrl <- findFileUrl
    whenJust maybeUrl $ \url ->
        redirect url
