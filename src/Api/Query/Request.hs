{-# LANGUAGE FlexibleContexts #-}
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
import Data.Maybe (mapMaybe)
import Data.Model.Request (Request)
import Database.Selda (restrict, select, (!), (.==))
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
requestQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "requests"
