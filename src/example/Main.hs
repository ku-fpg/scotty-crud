{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Monad.IO.Class (liftIO) 

import Data.Aeson
import Data.CRUD
import Data.HashMap.Strict as HashMap
import Data.Monoid (mconcat)

import Web.Scotty
import Web.Scotty.CRUD

main = scotty 3000 $ do

  users  <- liftIO $ actorCRUD (\ _ -> return ())
                      (HashMap.fromList [("foo",HashMap.fromList [("firstname", "Bla"),("lastname","Bllllab"),("age", Number 123)])] :: Table Row)

  scottyCRUD "/users" (atomicCRUD users)

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

