{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Request (requestQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common
  ( EntityTranslator,
    delimited,
    exact,
    fromEqual,
    idQualifier,
    literalName,
    parseDate,
    parseStatus,
    parseType,
    similar,
    undefinedQualifier,
  )
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.Model.Request (Request)
import Database.Selda (ascending, descending, order, restrict, select, (!), (.==))
import Database.Table (teams, users)

requestQueryTranslator :: EntityTranslator Request
requestQueryTranslator f (Literal txt) = literalName f txt
requestQueryTranslator f (Qualified "id" vals) = idQualifier f vals
requestQueryTranslator f (Qualified "author" vals) = do
  vs <- mapM (fromEqual "author") vals
  return $ \r -> do
    user <- select users
    similar id (user ! #name) vs
    restrict . f $ r ! #authorId .== user ! #_id
requestQueryTranslator f (Qualified "team" vals) = do
  vs <- mapM (fromEqual "team") vals
  return $ \r -> do
    team <- select teams
    similar id (team ! #name) vs
    restrict . f $ r ! #teamId .== team ! #_id
requestQueryTranslator f (Qualified "status" vals) = do
  vs <- mapM (fromEqual "status") vals
  return $ \r -> exact f (r ! #status) $ mapMaybe parseStatus vs
requestQueryTranslator f (Qualified "type" vals) = do
  vs <- mapM (fromEqual "type") vals
  return $ \r -> similar f (r ! #requestType) $ mapMaybe parseType vs
requestQueryTranslator f (Qualified "created" vals) =
  return $ \r -> delimited f (r ! #dateCreated) $ mapMaybe parseDate vals
requestQueryTranslator _ (Qualified "sort" vals) = do
  vs <- mapM (fromEqual "member") vals
  return $ \r -> forM_ (reverse vs) $ \case
    "title" -> order (r ! #name) ascending
    "title-asc" -> order (r ! #name) ascending
    "title-desc" -> order (r ! #name) descending
    "author" -> do
      user <- select users
      restrict $ r ! #authorId .== user ! #_id
      order (user ! #name) ascending
    "author-asc" -> do
      user <- select users
      restrict $ r ! #authorId .== user ! #_id
      order (user ! #name) ascending
    "author-desc" -> do
      user <- select users
      restrict $ r ! #authorId .== user ! #_id
      order (user ! #name) descending
    "team" -> do
      team <- select teams
      restrict $ r ! #teamId .== team ! #_id
      order (team ! #name) ascending
    "team-asc" -> do
      team <- select teams
      restrict $ r ! #teamId .== team ! #_id
      order (team ! #name) ascending
    "team-desc" -> do
      team <- select teams
      restrict $ r ! #teamId .== team ! #_id
      order (team ! #name) descending
    "created" -> order (r ! #dateCreated) ascending
    "created-asc" -> order (r ! #dateCreated) ascending
    "created-desc" -> order (r ! #dateCreated) descending
    "id" -> order (r ! #_id) ascending
    "id-asc" -> order (r ! #_id) ascending
    "id-desc" -> order (r ! #_id) descending
    "type" -> order (r ! #requestType) ascending
    "type-asc" -> order (r ! #requestType) ascending
    "type-desc" -> order (r ! #requestType) descending
requestQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "requests"
