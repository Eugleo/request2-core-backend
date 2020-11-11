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
import Data.Environment (EnvAction, param, rescue)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Network.HTTP.Types (badRequest400)


-- TODO Return the URL pointing to the next page
runQuery ::
    (ToJSON a, HasField "_id" a, FieldType "_id" a ~ ID a) =>
    Table a ->
    EntityTranslator (Inner PG) a ->
    EnvAction ()
runQuery tbl entityTranslator = do
    lim <- param "limit"
    offset <- param "offset"
    queryText <- param "query" `rescue` const (pure "")
    let queryBuilderOrError = do
            -- Defaultly sorts by descending id (i.e. newest on top)
            querySpec <- parseQuerySpec (queryText <> " sort:id-desc")
            let translate = makeQueryTranslator entityTranslator
            translate querySpec
    case queryBuilderOrError of
        Left err -> failure err badRequest400
        -- TODO Don't run the query twice
        Right queryBuilder -> do
            items <- query $
                limit offset lim $ do
                    item <- select tbl
                    queryBuilder item
                    return item
            res <- query . aggregate $ do
                item <- select tbl
                queryBuilder item
                return (count (item ! #_id))
            success (object ["values" .= toJSON items, "total" .= head res])


makeQueryTranslator :: EntityTranslator t a -> Translator t QuerySpecification a
makeQueryTranslator entityTranslator (Conjunction cs) = do
    -- Reverse, because of the order of multiple sort qualifiers
    -- The last one should have the least significance
    preds <- mapM clauseTranslator (reverse cs)
    return $ \t -> mapM_ ($ t) preds
  where
    clauseTranslator (Extant ent) = entityTranslator id ent
    clauseTranslator (Negated ent) = entityTranslator not_ ent
