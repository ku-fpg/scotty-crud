{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Web.Scotty.CRUD (
       -- * CRUD service
       scottyCRUD,
       -- * Basic types
       CRUD(..),
       Id, Table, Row, Named(..)
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

-- Scotty stuff
import Data.Aeson hiding (json)
import Web.Scotty as Scotty
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO) 
import Data.Monoid
import Network.HTTP.Types.Status (status204)
import Network.HTTP.Types ( StdMethod( OPTIONS ) )

------------------------------------------------------------------------------------
-- | A CRUD is a OO-style database Table of typed rows, with getters and setters. 
--   The default row is a JSON Object.
data CRUD m row = CRUD
     { createRow :: row       -> m (Named row)
     , getRow    :: Id        -> m (Maybe (Named row))
     , getTable  ::              m (Table row)
     , updateRow :: Named row -> m ()
     , deleteRow :: Id        -> m () -- alway works
     }
------------------------------------------------------------------------------------
-- Basic synonyms for key structures 
--
-- | Every (Named) Row must have an id field.
type Id        = Text

-- | a Table is a HashMap of Ids to rows, typically 'Table Row'.
--   The elems of the Table do not contain, by default, the Id, because this
--   is how you index a row. Note that the output of a complete table,
--   via RESTful CRUD, injects the Id into the row.

type Table row = HashMap Id row

-- | The default row is a aeson JSON object.
type Row       = Object

------------------------------------------------------------------------------------
-- | A pair of Name(Id) and row.
data Named row = Named Id row
   deriving (Eq,Show)

instance FromJSON row => FromJSON (Named row) where
    parseJSON (Object v) = Named
                <$> v .: "id"
                <*> (parseJSON $ Object $ HashMap.delete "id" v)
                
instance ToJSON row => ToJSON (Named row) where                
   toJSON (Named key row) = 
                   case toJSON row of
                     Object env -> Object $ HashMap.insert "id" (String key) env
                     _ -> error "row should be an object"



-- | scottyCRUD provides scotty support for a CRUD object.
-- 
-- > crud <- liftIO $ persistantCRUD "filename"
-- > scottyCRUD "URL" (atomicCRUD crud)

scottyCRUD :: (Show row, FromJSON row, ToJSON row) => String -> CRUD IO row -> ScottyM ()
scottyCRUD url crud = do
        let xRequest = do
               addHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type, Accep"
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
                liftIO $ print $ opt_row

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


--        get (capture $ url ++ "/:id") $ do return ()

                
