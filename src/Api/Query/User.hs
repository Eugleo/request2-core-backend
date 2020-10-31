{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.User (buildUserQuery) where

import Api.Query hiding (Query)
import qualified Api.Query as Api (Query)
import Api.Query.Common
import Data.Environment (EnvAction)
import Data.Maybe
import Data.Model.User
import Data.Text (append, pack)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Database.Table

buildUserQuery :: Api.Query -> EnvAction (Either Text [User])
buildUserQuery = buildQuery buildUserPredicate users

buildUserPredicate :: Api.Query -> QueryFor User
buildUserPredicate (Conjunction cs) =
  case mapM queryFromClause cs of
    Right preds -> Right $ \u -> mapM_ ($ u) preds
    Left message -> Left message

queryFromClause :: Clause -> QueryFor User
queryFromClause (Extant ent) = queryFromEntity ent id
queryFromClause (Negated ent) = queryFromEntity ent not_

-- TODO Implement Roles better, if possible
queryFromEntity :: Entity -> (Col PG Bool -> Col PG Bool) -> QueryFor User
queryFromEntity ent f =
  case ent of
    Literal txt ->
      return $ \u -> restrict . f $ singleSimilar (u ! #name) txt
    Qualified "dateCreated" vals ->
      return $ \u -> delimited f (u ! #dateCreated) $ mapMaybe parseDate vals
    Qualified "role" vals -> do
      vs <- mapM (fromEqual "role") vals
      return $ \u ->
        similar f (toString $ u ! #roles) $ mapMaybe (fmap (pack . show) . parseRole) vs
    Qualified "team" vals -> do
      vs <- mapM (fromEqual "team") vals
      return $ \u -> do
        team <- select teams
        similar id (team ! #name) vs
        restrict . f $ u ! #teamId .== team ! #_id
    Qualified "name" vals -> do
      vs <- mapM (fromEqual "name") vals
      return $ \u -> similar f (u ! #name) vs
    Qualified "email" vals -> do
      vs <- mapM (fromEqual "email") vals
      return $ \u -> similar f (u ! #email) vs
    Qualified "active" vals -> do
      vs <- mapM (fromEqual "active") vals
      return $ \u -> exact f (u ! #active) $ mapMaybe parseBool vs
    Qualified "id" vals ->
      return $ \u -> delimited f (u ! #_id) $ mapMaybe parseId vals
    Qualified quant _ ->
      Left ("The quantifier " `append` quant `append` " is not defined for users")