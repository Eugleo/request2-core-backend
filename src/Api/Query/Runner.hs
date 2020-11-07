{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Query.Runner where

import Api.Common (failure, success)
import Api.Query (Clause (Extant, Negated), QuerySpecification (..))
import Api.Query.Common (EntityTranslator, Translator)
import Api.Query.Parser (parseQuerySpec)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Environment (EnvAction, param)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Network.HTTP.Types (badRequest400)

runQuery ::
  (ToJSON a, HasField "_id" a, FieldType "_id" a ~ ID a) =>
  Table a ->
  EntityTranslator (Inner PG) a ->
  EnvAction ()
runQuery tbl entityTranslator = do
  lim <- param "limit"
  offset <- param "offset"
  queryText <- param "query"
  let queryBuilderOrError = do
        querySpec <- parseQuerySpec queryText
        let translate = makeQueryTranslator entityTranslator
        translate querySpec
  case queryBuilderOrError of
    Left err -> failure err badRequest400
    Right queryBuilder -> do
      items <- query $
        limit offset lim $ do
          item <- select tbl
          queryBuilder item
          return item
      res <- query . aggregate $ do
        v <- select tbl
        return (count (v ! #_id))
      success (object ["values" .= toJSON items, "total" .= head res])

makeQueryTranslator :: EntityTranslator t a -> Translator t QuerySpecification a
makeQueryTranslator entityTranslator (Conjunction cs) = do
  preds <- mapM clauseTranslator cs
  return $ \t -> mapM_ ($ t) preds
  where
    clauseTranslator (Extant ent) = entityTranslator id ent
    clauseTranslator (Negated ent) = entityTranslator not_ ent