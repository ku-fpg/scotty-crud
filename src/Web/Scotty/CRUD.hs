{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Web.Scotty.CRUD (
       -- * CRUD Service
       scottyCRUD,
       -- * Basic Types
       CRUD(..),
       Id, Table, Row, Named(..)
       ) where


import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid

import           Control.Monad.IO.Class (liftIO)

import           Network.HTTP.Types.Status (status204)
import           Network.HTTP.Types ( StdMethod( OPTIONS ) )

import           Web.Scotty as Scotty
import           Web.Scotty.CRUD.Types

-- | scottyCRUD provides scotty support for a CRUD object.
--
-- > crud <- liftIO $ persistentCRUD "filename"
-- > scottyCRUD "URL" crud

scottyCRUD :: (Show row, FromJSON row, ToJSON row) => String -> CRUD row -> ScottyM ()
scottyCRUD url crud = do
        let xRequest = do
               addHeader "Access-Control-Allow-Headers" "Authorization, Origin, X-Requested-With, Content-Type, Accep"
               addHeader "Access-Control-Allow-Methods" "POST, GET, PUT, DELETE, OPTIONS"
               addHeader "Access-Control-Allow-Origin"  "*"

        post (capture url) $ do
                xRequest
                dat <- jsonData
                namedRow <- liftIO $ createRow crud dat
                Scotty.json namedRow

        get (capture url) $ do
                xRequest
                tab <- liftIO $ getTable crud
                Scotty.json $ [ Named k v | (k,v) <- HashMap.toList $ tab ]

        get (capture (url <> "/:id")) $ do
                xRequest
                iD <- param "id"
                opt_row <- liftIO $ getRow crud iD
                case opt_row of
                  Nothing -> next
                  Just namedRow -> Scotty.json $ namedRow

        put (capture (url <> "/:id")) $ do
                xRequest
                dat <- jsonData
                () <- liftIO $ updateRow crud dat
                Scotty.json dat

        delete (capture (url <> "/:id")) $ do
                xRequest
                iD <- param "id"
                () <- liftIO $ deleteRow crud iD
                status $ status204
                raw ""

        addroute OPTIONS (capture url) $ do
                xRequest
                text "OK"

        addroute OPTIONS (capture (url <> "/:id")) $ do
                xRequest
                text "OK"
