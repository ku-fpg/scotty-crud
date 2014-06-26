{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web.Scotty.CRUD where

import Data.Aeson hiding (json)
import Web.Scotty 
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO) 
import Data.CRUD
import Data.Monoid

-- | scottyCRUD provides scotty support for a CRUD object.
-- 
-- > crud <- liftIO $ persistantCRUD "filename"
-- > scottyCRUD "URL" (atomicCRUD crud)

scottyCRUD :: (Show row, FromJSON row, ToJSON row) => String -> CRUD IO row -> ScottyM ()
scottyCRUD url crud = do

        post (capture url) $ do
                dat <- jsonData
                namedRow <- liftIO $ createRow crud dat
                json namedRow

        get (capture url) $ do 
                tab <- liftIO $ getTable crud
                json $ [ Named k v | (k,v) <- HashMap.toList $ tab ]
        
        get (capture (url <> "/:id")) $ do 
                iD <- param "id"
                opt_row <- liftIO $ getRow crud iD
                case opt_row of
                  Nothing -> next
                  Just namedRow -> json $ namedRow
                liftIO $ print $ opt_row

--        get (capture $ url ++ "/:id") $ do return ()

                
