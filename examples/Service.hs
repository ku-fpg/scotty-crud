{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Web.Scotty
import Data.Aeson
import Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO) 

import Web.Scotty.CRUD
import Web.Scotty.CRUD.Persistant (actorCRUD, atomicCRUD)

import Data.Monoid (mconcat)

main = scotty 3000 $ do

  users  <- liftIO $ actorCRUD (\ _ -> return ())
                      (HashMap.fromList [("foo",HashMap.fromList [("firstname", "Roger"),("lastname","Rabbit"),("age", Number 21)])] :: Table Row)

  scottyCRUD "/users" (atomicCRUD users)


