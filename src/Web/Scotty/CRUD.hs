{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web.Scotty.CRUD where

import Data.Aeson hiding (json)
import Web.Scotty 
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO) 
import Data.CRUD
import Data.Monoid
import Network.HTTP.Types.Status (status204)
import Network.HTTP.Types ( StdMethod( OPTIONS ) )

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
                json namedRow

        get (capture url) $ do 
                xRequest
                tab <- liftIO $ getTable crud
                json $ [ Named k v | (k,v) <- HashMap.toList $ tab ]
        
        get (capture (url <> "/:id")) $ do 
                xRequest
                iD <- param "id"
                opt_row <- liftIO $ getRow crud iD
                case opt_row of
                  Nothing -> next
                  Just namedRow -> json $ namedRow
                liftIO $ print $ opt_row

        put (capture (url <> "/:id")) $ do 
                xRequest
                dat <- jsonData
                () <- liftIO $ updateRow crud dat
                json dat
                
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

                
