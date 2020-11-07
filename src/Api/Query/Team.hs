{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Team (teamQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common
  ( EntityTranslator,
    activeQualifier,
    fromEqual,
    idQualifier,
    literalName,
    makeSorter,
    similar,
    undefinedQualifier,
  )
import Control.Monad (forM, forM_)
import Data.Model.Team (Team)
import Database.Selda
  ( ascending,
    descending,
    order,
    restrict,
    select,
    (!),
    (.==),
  )
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
teamQueryTranslator f (Qualified "active" vals) = activeQualifier f vals
teamQueryTranslator f (Qualified "id" vals) = idQualifier f vals
-- teamQueryTranslator _ (Qualified "sort" vals) = do
--   vs <- mapM (fromEqual "member") vals
--   s <-
--     forM (reverse vs) $
--       makeSorter
--         [ ("name", #name),
--           ("code", #code),
--           ("id", #_id),
--           ("active", #active)
--         ]
--   return $ \t -> sequence_ $ s <*> [t]
teamQueryTranslator _ (Qualified "sort" vals) = do
  vs <- mapM (fromEqual "member") vals
  return $ \t -> forM_ (reverse vs) $ \case
    "name" -> order (t ! #name) ascending
    "name-asc" -> order (t ! #name) ascending
    "name-desc" -> order (t ! #name) descending
    "code" -> order (t ! #code) ascending
    "code-asc" -> order (t ! #code) ascending
    "code-desc" -> order (t ! #code) descending
    "id" -> order (t ! #_id) ascending
    "id-asc" -> order (t ! #_id) ascending
    "id-desc" -> order (t ! #_id) descending
    "active" -> order (t ! #active) ascending
    "active-asc" -> order (t ! #active) ascending
    "active-desc" -> order (t ! #active) descending
teamQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "teams"
