{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.User (userQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common
  ( EntityTranslator,
    activeQualifier,
    delimited,
    fromEqual,
    idQualifier,
    literalName,
    parseDate,
    parseRole,
    similar,
    undefinedQualifier,
  )
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.Model.User (User)
import Data.Text (pack)
import Database.Selda (ascending, descending, order, restrict, select, toString, (!), (.==))
import Database.Table (teams)

-- TODO Implement Roles better, if possible
userQueryTranslator :: EntityTranslator User
userQueryTranslator f (Literal txt) = literalName f txt
userQueryTranslator f (Qualified "dateCreated" vals) =
  return $ \u -> delimited f (u ! #dateCreated) $ mapMaybe parseDate vals
userQueryTranslator f (Qualified "role" vals) = do
  vs <- mapM (fromEqual "role") vals
  return $ \u ->
    similar f (toString $ u ! #roles) $ mapMaybe (fmap (pack . show) . parseRole) vs
userQueryTranslator f (Qualified "team" vals) = do
  vs <- mapM (fromEqual "team") vals
  return $ \u -> do
    team <- select teams
    similar id (team ! #name) vs
    restrict . f $ u ! #teamId .== team ! #_id
userQueryTranslator f (Qualified "name" vals) = do
  vs <- mapM (fromEqual "name") vals
  return $ \u -> similar f (u ! #name) vs
userQueryTranslator f (Qualified "email" vals) = do
  vs <- mapM (fromEqual "email") vals
  return $ \u -> similar f (u ! #email) vs
userQueryTranslator f (Qualified "active" vals) = activeQualifier f vals
userQueryTranslator f (Qualified "id" vals) = idQualifier f vals
userQueryTranslator _ (Qualified "sort" vals) = do
  vs <- mapM (fromEqual "member") vals
  return $ \u -> forM_ (reverse vs) $ \case
    "name" -> order (u ! #name) ascending
    "name-asc" -> order (u ! #name) ascending
    "name-desc" -> order (u ! #name) descending
    "email" -> order (u ! #email) ascending
    "email-asc" -> order (u ! #email) ascending
    "email-desc" -> order (u ! #email) descending
    "created" -> order (u ! #dateCreated) ascending
    "created-asc" -> order (u ! #dateCreated) ascending
    "created-desc" -> order (u ! #dateCreated) descending
    "id" -> order (u ! #_id) ascending
    "id-asc" -> order (u ! #_id) ascending
    "id-desc" -> order (u ! #_id) descending
    "active" -> order (u ! #active) ascending
    "active-asc" -> order (u ! #active) ascending
    "active-desc" -> order (u ! #active) descending
userQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "users"
