module Api.Files where

--import Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
--import qualified Database.Common as Db
--import Database.Selda hiding (update)
--import Network.HTTP.Types.Status (created201, ok200)
--import Utils.Id.AddId
--import qualified Database.Selda as Selda (update)

import Data.Environment
import Network.HTTP.Types.Status (notFound404)
import Network.Wai.Parse (FileInfo (..))

upload :: EnvAction ()
upload = do
  receivedFiles <- files
  case receivedFiles of
    [(_, info)] -> do
      envIO $ print $ fileName info
    _ -> status notFound404
