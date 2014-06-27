{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Web.Scotty.CRUD
import Web.Scotty.CRUD.Persistant
import System.IO

-- Read from stdin, send to stdout
main = do
        -- Read what you can, please, into a Table.
        tab :: Table Row <- readTable stdin
        writeTable stdout tab
