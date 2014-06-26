{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Web.Scotty
import Data.Aeson
import Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO) 

import Web.Scotty.CRUD
import Data.CRUD

import Data.Monoid (mconcat)

main = scotty 3000 $ do

  users  <- liftIO $ actorCRUD (\ _ -> return ())
                      (HashMap.fromList [("foo",HashMap.fromList [("x", Number 123)])] :: Table Row)

  scottyCRUD "/users" (atomicCRUD users)

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

