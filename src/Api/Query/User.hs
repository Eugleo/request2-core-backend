{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.User (userQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common
  ( EntityTranslator,
    activeQuantifier,
    delimited,
    fromEqual,
    idQuantifier,
    literalName,
    parseDate,
    parseRole,
    similar,
    undefinedQuantifier,
  )
import Data.Maybe (mapMaybe)
import Data.Model.User (User)
import Data.Text (pack)
import Database.Selda (restrict, select, toString, (!), (.==))
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
userQueryTranslator f (Qualified "active" vals) = activeQuantifier f vals
userQueryTranslator f (Qualified "id" vals) = idQuantifier f vals
userQueryTranslator _ (Qualified quant _) = undefinedQuantifier quant "users"
