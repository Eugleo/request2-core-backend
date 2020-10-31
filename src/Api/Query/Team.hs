{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Team (buildTeamQuery) where

import Api.Query hiding (Query)
import qualified Api.Query as Api (Query)
import Api.Query.Common
import Data.Environment (EnvAction)
import Data.Maybe
import Data.Model.Team
import Data.Text (append)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Database.Table

type FilteringQuery = Either Text (Row PG Team -> Query PG ())

buildTeamQuery :: Api.Query -> EnvAction (Either Text [Team])
buildTeamQuery = buildQuery buildTeamPredicate teams

buildTeamPredicate :: Api.Query -> FilteringQuery
buildTeamPredicate (Conjunction cs) =
  case mapM queryFromClause cs of
    Right preds -> Right $ \t -> mapM_ ($ t) preds
    Left message -> Left message

queryFromClause :: Clause -> FilteringQuery
queryFromClause (Extant ent) = queryFromEntity ent id
queryFromClause (Negated ent) = queryFromEntity ent not_

queryFromEntity :: Entity -> (Col PG Bool -> Col PG Bool) -> FilteringQuery
queryFromEntity ent f =
  case ent of
    Literal txt ->
      return $ \t -> restrict . f $ singleSimilar (t ! #name) txt
    Qualified "member" vals -> do
      vs <- mapM (fromEqual "member") vals
      return $ \t -> do
        user <- select users
        similar id (user ! #name) vs
        restrict . f $ t ! #_id .== user ! #teamId
    Qualified "name" vals -> do
      vs <- mapM (fromEqual "name") vals
      return $ \t -> similar f (t ! #name) vs
    Qualified "code" vals -> do
      vs <- mapM (fromEqual "code") vals
      return $ \t -> similar f (t ! #code) vs
    Qualified "active" vals -> do
      vs <- mapM (fromEqual "active") vals
      return $ \t -> exact f (t ! #active) $ mapMaybe parseBool vs
    Qualified "id" vals ->
      return $ \t -> delimited f (t ! #_id) $ mapMaybe parseId vals
    Qualified quant _ ->
      Left ("The quantifier " `append` quant `append` " is not defined for teams")