{-# LANGUAGE RankNTypes, TypeOperators, OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Web.Scotty.CRUD (
       scottyCRUD, CRUD(..), Crud(..)
       ) where

import qualified  Control.Object as O
import            Control.Transformation ((#))

import           Data.Aeson
import           Data.Monoid
import           Data.Text(Text)
import qualified Data.Text.Lazy as L

import           Control.Monad.IO.Class (liftIO)

import           Network.HTTP.Types.Status (status204)
import           Network.HTTP.Types ( StdMethod( OPTIONS ) )

import qualified Web.Scotty as Scotty
import           Web.Scotty (capture, param, jsonData, ScottyM,raw)


-- | scottyCRUD provides scotty support for a CRUD object.
--
-- > crud <- liftIO $ persistentCRUD "filename"
-- > scottyCRUD "URL" crud

class Crud f where
  create :: Value                  -> f Value
  get    :: Text                   -> f (Maybe Value)
  table  ::          [(Text,Text)] -> f [Value]
  update :: Value                  -> f ()
  delete :: Text                   -> f ()

data CRUD :: * -> * where
  Create :: Value                  -> CRUD Value
  Get    :: Text                   -> CRUD (Maybe Value)
  Table  ::          [(Text,Text)] -> CRUD [Value]
  Update :: Value                  -> CRUD ()
  Delete :: Text                   -> CRUD ()
        
instance Crud CRUD where
  create = Create
  get    = Get
  table  = Table
  update = Update
  delete = Delete

scottyCRUD :: Crud f => String -> O.Object f -> ScottyM ()
scottyCRUD url crud = do
        let xRequest = do
               Scotty.addHeader "Access-Control-Allow-Headers" "Authorization, Origin, X-Requested-With, Content-Type, Accep"
               Scotty.addHeader "Access-Control-Allow-Methods" "POST, GET, PUT, DELETE, OPTIONS"
               Scotty.addHeader "Access-Control-Allow-Origin"  "*"

        Scotty.post (capture url) $ do
                xRequest
                dat <- jsonData
                namedRow <- liftIO $ crud # create dat
                Scotty.json namedRow

        Scotty.get (capture url) $ do
                xRequest
                ps <- fmap (\ (a,b) -> (L.toStrict a, L.toStrict b)) <$> Scotty.params
                tab <- liftIO $ crud # table ps
                Scotty.json tab

        Scotty.get (capture (url <> "/:id")) $ do
                xRequest
                iD <- param "id"
                opt_row <- liftIO $ crud # get iD
                case opt_row of
                  Nothing -> Scotty.next
                  Just namedRow -> Scotty.json $ namedRow

        Scotty.put (capture (url <> "/:id")) $ do
                xRequest
                dat <- Scotty.jsonData
                () <- liftIO $ crud # update dat
                Scotty.json dat

        Scotty.delete (capture (url <> "/:id")) $ do
                xRequest
                iD <- param "id"
                () <- liftIO $ crud # delete iD
                Scotty.status $ status204
                raw ""

        Scotty.addroute OPTIONS (capture url) $ do
                xRequest
                Scotty.text "OK"

        Scotty.addroute OPTIONS (capture (url <> "/:id")) $ do
                xRequest
                Scotty.text "OK"


