module Api.Query.Runner where

import Api.Common (failure, success)
import Api.Query
import qualified Api.Query.Parser as Parse
import Data.Aeson (ToJSON)
import Data.Environment
import Data.Text
import Network.HTTP.Types (badRequest400)
import Text.Megaparsec (runParser)

runSearchQuery :: ToJSON a => (Query -> EnvAction (Either Text a)) -> EnvAction ()
runSearchQuery runQuery = do
  queryText <- param "query"
  let eq = runParser Parse.query "" queryText
  case eq of
    Left _ -> failure "Query parsing failed" badRequest400
    Right q -> do
      itemsOrError <- runQuery q
      case itemsOrError of
        Left message -> failure message badRequest400
        Right items -> success items