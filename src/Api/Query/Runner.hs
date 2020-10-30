module Api.Query.Runner where

import Api.Common (success)
import Api.Query
import qualified Api.Query.Parser as Parse
import Data.Aeson (ToJSON)
import Data.Environment
import Network.HTTP.Types (badRequest400)
import Text.Megaparsec

runSearchQuery :: ToJSON a => (Query -> EnvAction a) -> EnvAction ()
runSearchQuery runQuery = do
  queryText <- (param "query")
  let eq = runParser Parse.query "" queryText
  case eq of
    Left _ -> status badRequest400
    Right q -> do
      items <- runQuery q
      success items