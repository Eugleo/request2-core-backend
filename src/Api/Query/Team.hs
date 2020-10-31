{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Team (teamQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common
  ( EntityTranslator,
    activeQuantifier,
    fromEqual,
    idQuantifier,
    literalName,
    similar,
    undefinedQuantifier,
  )
import Data.Model.Team (Team)
import Database.Selda (restrict, select, (!), (.==))
import Database.Table (users)

teamQueryTranslator :: EntityTranslator Team
teamQueryTranslator f (Literal txt) = literalName f txt
teamQueryTranslator f (Qualified "member" vals) = do
  vs <- mapM (fromEqual "member") vals
  return $ \t -> do
    user <- select users
    similar id (user ! #name) vs
    restrict . f $ t ! #_id .== user ! #teamId
teamQueryTranslator f (Qualified "name" vals) = do
  vs <- mapM (fromEqual "name") vals
  return $ \t -> similar f (t ! #name) vs
teamQueryTranslator f (Qualified "code" vals) = do
  vs <- mapM (fromEqual "code") vals
  return $ \t -> similar f (t ! #code) vs
teamQueryTranslator f (Qualified "active" vals) = activeQuantifier f vals
teamQueryTranslator f (Qualified "id" vals) = idQuantifier f vals
teamQueryTranslator _ (Qualified quant _) = undefinedQuantifier quant "teams"
