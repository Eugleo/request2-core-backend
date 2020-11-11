{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Announcement (annQueryTranslator) where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common (
    EntityTranslator,
    GenericSelector (..),
    activeQualifier,
    delimited,
    fromEqual,
    makeSorter,
    orderBy,
    parseDate,
    similar,
    singleSimilar,
    undefinedQualifier,
 )
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import Data.Model.Ann (Ann)
import Database.Selda (order, restrict, select, (!), (.==))
import Database.Table (users)


annQueryTranslator :: EntityTranslator t Ann
annQueryTranslator f (Literal txt) =
    return $ \a -> restrict . f $ singleSimilar (a ! #title) txt
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
    sorters <-
        forM (reverse vs) $
            makeSorter
                [ ("title", orderBy (ToSelector #title)),
                  ("created", orderBy (ToSelector #dateCreated)),
                  ("id", orderBy (ToSelector #_id)),
                    ( "author",
                      \ordering a -> do
                        user <- select users
                        restrict $ a ! #authorId .== user ! #_id
                        order (user ! #name) ordering
                    ),
                  ("active", orderBy (ToSelector #active))
                ]
    return $ \t -> sequence_ $ sorters <*> [t]
annQueryTranslator _ (Qualified quant _) = undefinedQualifier quant "announcements"
