{-# LANGUAGE FlexibleContexts #-}
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
import Data.Maybe (mapMaybe)
import Data.Model.Ann (Ann)
import Database.Selda (restrict, select, (!), (.==))
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
annQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "announcements"
