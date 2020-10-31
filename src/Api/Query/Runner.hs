{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Query.Runner where

import Api.Common (failure, success)
import Api.Query (Clause (Extant, Negated), QuerySpecification (..))
import Api.Query.Common (EntityTranslator, Translator)
import Api.Query.Parser (parseQuerySpec)
import Data.Aeson (ToJSON)
import Data.Environment (EnvAction, param)
import Database.Selda (Relational, Table, not_, query, select)
import Network.HTTP.Types (badRequest400)

runQuery :: (ToJSON a, Relational a) => Table a -> EntityTranslator a -> EnvAction ()
runQuery tbl entityTranslator = do
  queryText <- param "query"
  let queryBuilderOrError = do
        querySpec <- parseQuerySpec queryText
        let translate = makeQueryTranslator entityTranslator
        translate querySpec
  case queryBuilderOrError of
    Left err -> failure err badRequest400
    Right queryBuilder -> do
      items <- query $ do
        item <- select tbl
        queryBuilder item
        return item
      success items

makeQueryTranslator :: EntityTranslator a -> Translator QuerySpecification a
makeQueryTranslator entityTranslator (Conjunction cs) = do
  preds <- mapM clauseTranslator cs
  return $ \t -> mapM_ ($ t) preds
  where
    clauseTranslator (Extant ent) = entityTranslator id ent
    clauseTranslator (Negated ent) = entityTranslator not_ ent