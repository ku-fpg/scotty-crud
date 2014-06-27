{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Web.Scotty.CRUD.SQL (
         -- * SQL-style SELECT
         SELECT(..),
         SORT_MOD(..),
         select 
         ) where

import Data.Aeson
import Data.Text (Text, pack)
import Control.Monad

import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO) 
import Data.Monoid
import Network.HTTP.Types.Status (status204)
import Network.HTTP.Types ( StdMethod( OPTIONS ) )

import Web.Scotty as Scotty

import Web.Scotty.CRUD.Types
 
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
