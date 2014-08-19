{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class (liftIO) 

import Data.Monoid

import Data.Text (Text, pack)
import Data.Aeson
import Data.HashMap.Strict as HashMap

import Web.Scotty
import Web.Scotty.CRUD
import Web.Scotty.CRUD.JSON (actorCRUD)

main :: IO ()
main = scotty 3000 $ do

  let tab :: Table Row
      tab = HashMap.fromList $
      	    [("foo",HashMap.fromList [("firstname", "Roger"),("lastname","Rabbit"),("age", Number 21)])
	    ,("abc",HashMap.fromList [("firstname", "Roger"),("lastname","Rabbit"),("bla",Bool True)])
	    ] ++
	    [ ("abc-" <> pack (show n),HashMap.fromList [("firstname", "Roger"),("lastname","Rabbit"),("bla",Bool False)])
	    | n <- [1..100]
	    ]

  users  <- liftIO $ actorCRUD 
  	    	      (\ _ -> return ())	-- do not store the updates anywhere
	      tab     	     		-- but supply an (updatable) table

  scottyCRUD "/users" users


