{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Web.Scotty
import Data.Aeson
import Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO) 

import Web.Scotty.CRUD
import Web.Scotty.CRUD.JSON (actorCRUD)

import Data.Monoid (mconcat)

main = scotty 3000 $ do

  let tab :: Table Row
      tab = HashMap.fromList [("foo",HashMap.fromList [("firstname", "Roger"),("lastname","Rabbit"),("age", Number 21)])]

  users  <- liftIO $ actorCRUD 
  	    	      (\ _ -> return ())	-- do not store the updates anywhere
	      tab     	     		-- but supply an (updatable) table

  scottyCRUD "/users" users


