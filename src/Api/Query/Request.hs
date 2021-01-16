{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Request where

import Api.Query (Entity (Literal, Qualified))
import Api.Query.Common (
    EntityTranslator,
    GenericSelector (..),
    delimited,
    exact,
    fromEqual,
    literalName,
    makeSorter,
    orderBy,
    parseDate,
    parseStatus,
    parseType,
    similar,
    singleSimilar,
    undefinedQualifier,
 )
import Control.Monad (forM)
import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import Data.Model.Request (Request)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Database.Selda (ID, order, restrict, select, toId, (!), (.==))
import Database.Table (teams, users)


requestQueryTranslator :: EntityTranslator t Request
requestQueryTranslator f (Literal txt) =
    return $ \r -> restrict . f $ singleSimilar (r ! #title) txt
requestQueryTranslator f (Qualified "id" vals) =
    return $ \r -> delimited f (r ! #_id) $ mapMaybe parseId vals
requestQueryTranslator f (Qualified "code" vals) =
    return $ \r -> delimited f (r ! #_id) $ mapMaybe parseId vals
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
    vs <- mapM (fromEqual "sort") vals
    sorters <-
        forM (reverse vs) $
            makeSorter
                [ ("title", orderBy (ToSelector #title)),
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


-- DON'T CHANGE THIS UNLESS YOU CHANGE /Request/Request.ts AS WELL
parseId :: Traversable t => t Text -> Maybe (t (ID Request))
parseId = traverse (fmap toId . go (0 :: Int) . unpack . T.reverse . T.toUpper)
  where
    alphabetSize = length alphabet
    alphabet = "123456789ABCDEFGHIJKLMNPQRSTUVWXYZ"
    toCodePoint = flip elemIndex alphabet
    go _ [] = Just 0
    go i (c : cs) = do
        digit <- toCodePoint c
        rest <- go (i + 1) cs
        return $ rest + digit * (alphabetSize ^ i)
