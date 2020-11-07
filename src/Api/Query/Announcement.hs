{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Announcement (annQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common
  ( EntityTranslator,
    activeQualifier,
    delimited,
    fromEqual,
    idQualifier,
    parseDate,
    similar,
    singleSimilar,
    undefinedQualifier,
  )
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.Model.Ann (Ann)
import Database.Selda (ascending, descending, order, restrict, select, (!), (.==))
import Database.Table (users)

annQueryTranslator :: EntityTranslator Ann
annQueryTranslator f (Literal txt) =
  return $ \a -> restrict . f $ singleSimilar (a ! #title) txt
annQueryTranslator f (Qualified "id" vals) = idQualifier f vals
annQueryTranslator f (Qualified "content" vals) = do
  vs <- mapM (fromEqual "content") vals
  return $ \a -> similar f (a ! #body) vs
annQueryTranslator f (Qualified "author" vals) = do
  vs <- mapM (fromEqual "author") vals
  return $ \a -> do
    user <- select users
    similar id (user ! #name) vs
    restrict . f $ a ! #authorId .== user ! #_id
annQueryTranslator f (Qualified "created" vals) =
  return $ \a -> delimited f (a ! #dateCreated) $ mapMaybe parseDate vals
annQueryTranslator f (Qualified "active" vals) = activeQualifier f vals
annQueryTranslator _ (Qualified "sort" vals) = do
  vs <- mapM (fromEqual "member") vals
  return $ \a -> forM_ (reverse vs) $ \case
    "title" -> order (a ! #title) ascending
    "title-asc" -> order (a ! #title) ascending
    "title-desc" -> order (a ! #title) descending
    "author" -> do
      user <- select users
      restrict $ a ! #authorId .== user ! #_id
      order (user ! #name) ascending
    "author-asc" -> do
      user <- select users
      restrict $ a ! #authorId .== user ! #_id
      order (user ! #name) ascending
    "author-desc" -> do
      user <- select users
      restrict $ a ! #authorId .== user ! #_id
      order (user ! #name) descending
    "created" -> order (a ! #dateCreated) ascending
    "created-asc" -> order (a ! #dateCreated) ascending
    "created-desc" -> order (a ! #dateCreated) descending
    "id" -> order (a ! #_id) ascending
    "id-asc" -> order (a ! #_id) ascending
    "id-desc" -> order (a ! #_id) descending
    "active" -> order (a ! #active) ascending
    "active-asc" -> order (a ! #active) ascending
    "active-desc" -> order (a ! #active) descending
annQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "announcements"
