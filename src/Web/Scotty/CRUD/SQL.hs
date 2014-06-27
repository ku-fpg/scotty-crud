{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Web.Scotty.CRUD.SQL (
         -- * SQL-style SELECT
         SELECT(..),
         SORT_MOD(..),
         select 
         ) where

import Data.Aeson
import Data.Aeson.Parser as P
import Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Applicative
import Data.Char (isSpace, isDigit, chr)
import Data.List (foldl', sortBy)
import Data.Text (Text, pack)
import Control.Monad
import qualified Data.Text as Text
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import System.IO
import Data.Scientific

import Web.Scotty.CRUD
 
------------------------------------------------------------------------------------
-- SQL-style SELECT

-- | SQL-style DSL
data SELECT = SELECT [Text]                    -- ^ Only return listed fields
            | SORT_BY Text [SORT_MOD]          -- ^ sort on a field
            | TAKE Int                         -- ^ Only give n answers
            | DROP Int                         -- ^ ignore the first n answers
            deriving (Eq, Ord, Show, Read)

-- | execute the SQL DSL, from left to right.
select :: [SELECT] -> [Row] -> [Row]
select ss rows = foldl sel rows ss
  where sel rows (SELECT ns)     = fmap (\ row -> HashMap.fromList [ (k,v) | (k,v) <- HashMap.toList row, k `elem` ns]) rows
        sel rows (SORT_BY nm ms) 
                | NUM `elem` ms && DESC `elem` ms = sort_by desc $ prep num 
                | NUM `elem` ms                   = sort_by desc $ prep num 
                |                  DESC `elem` ms = sort_by asc  $ prep txt
                | otherwise                       = sort_by asc  $ prep txt 
          where 
                 asc  a b = fst a `compare` fst b
                 desc a b = fst b `compare` fst a

                 num :: Row -> Maybe Scientific
                 num row = case HashMap.lookup nm row of
                            Just (Number n)   -> return n
                            Just (String txt) -> case reads $ Text.unpack txt of
                                                   [(a::Scientific,"")] -> Just a
                                                   _                    -> Nothing
                            _                 -> Nothing

                 txt :: Row -> Maybe Text
                 txt row = case HashMap.lookup nm row of
                            Just (String txt) -> return txt
                            Just (Number n)   -> return $ Text.pack $ show n
                            Just (Bool b)     -> return $ Text.pack $ show b
                            _                 -> Nothing

                 sort_by :: (Ord a) => ((a,b) -> (a,b) -> Ordering) -> [(a,b)] -> [b]
                 sort_by cmp = map snd . sortBy cmp

                 prep :: forall a . (Row -> a) -> [(a,Row)]                      
                 prep p = map (\ v -> (p v,v)) rows
                              

data SORT_MOD = NUM | DESC
        deriving (Eq, Ord, Show, Read)
