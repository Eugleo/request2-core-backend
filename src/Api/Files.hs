module Api.Files where

import Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
import Data.Environment
import Data.Maybe (listToMaybe)
import qualified Database.Common as Db
import Database.Selda hiding (update)
import qualified Database.Selda as Selda (update)
import Network.HTTP.Types.Status (created201, notFound404, ok200)
import Network.Wai.Parse (FileInfo (..))
import Utils.Id.AddId

upload :: EnvAction ()
upload = do
  receivedFiles <- files
  case receivedFiles of
    [(x, info)] -> do
      liftIO $ print $ fileName info
    _ -> status notFound404
