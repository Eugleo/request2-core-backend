{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Request (requestQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common
  ( EntityTranslator,
    GenericSelector (..),
    delimited,
    exact,
    fromEqual,
    idQualifier,
    literalName,
    makeSorter,
    orderBy,
    parseDate,
    parseStatus,
    parseType,
    similar,
    undefinedQualifier,
  )
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import Data.Model.Request (Request)
import Database.Selda (order, restrict, select, (!), (.==))
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
  sorters <-
    forM (reverse vs) $
      makeSorter
        [ ("title", orderBy (ToSelector #name)),
          ("created", orderBy (ToSelector #dateCreated)),
          ("id", orderBy (ToSelector #_id)),
          ("type", orderBy (ToSelector #requestType)),
          ("status", orderBy (ToSelector #status)),
          ( "author",
            \ordering r -> do
              user <- select users
              restrict $ r ! #authorId .== user ! #_id
              order (user ! #name) ordering
          ),
          ( "team",
            \ordering r -> do
              team <- select teams
              restrict $ r ! #teamId .== team ! #_id
              order (team ! #name) ordering
          )
        ]
  return $ \t -> sequence_ $ sorters <*> [t]
requestQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "requests"
