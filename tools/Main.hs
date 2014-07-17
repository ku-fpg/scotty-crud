{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Web.Scotty.CRUD
import Web.Scotty.CRUD.JSON
import Web.Scotty.CRUD.Types
import System.IO
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Char (chr, isDigit)
import System.Environment
import Web.Scotty as Scotty hiding (raw)
import Control.Monad.IO.Class (liftIO) 

main :: IO ()
main = do
        args <- getArgs
        let (flags,opts) = span ("--" `isPrefixOf`) args
        case opts of
          ("compress":opts') | null flags -> compress_main opts'
          ("table":opts')    | null opts' -> compress_main flags
          ("update":opts')                -> update_main   flags opts'                            
          ("server":opts')                -> server_main   flags opts'                            
          _ -> error $ unlines
                [ "usage: crud [options] [command] [files]"
                , "         where command = compress | table | update | delta | server"
                , ""
                , "  -- compress a db"
                , "  crud compress < input.json > compressed-output.json"
                , "  crud compress db.json"
                , ""
                , "  -- output an ASCII table"
                , "  crud [--'<width>'] table < input.json | less"
                , "        --<width> : max width of each column (default 20)"
                , ""
                , "  -- update a db"
                , "  crud [--sub] update db.json < new.json"
                , "        --sub : sub-record merging"
                , ""
                , "  -- find the differences between a db and a new-db"
                , "  crud [--sub] diff db.json new-db.json"
                , "        --sub : generate a sub-record diff"
                , ""
                , "  -- serve up a set of db's (names and URL paths are synonymous)"
                ,"   crud [--read-only|--local] server 3000 db.json [db2.json ...]"
                ]

------------------------------------------------------------------------------------------------------------

compress_main :: [String] -> IO ()
compress_main [] = do
        tab :: Table Row <- readTable stdin
        writeTable stdout tab
compress_main [db] = do
        h <- openBinaryFile db ReadMode
        tab :: Table Row <- readTable h
        hClose h
        h <- openBinaryFile db WriteMode
        writeTable h tab
        hClose h
compress_main _ = error "crud compress: unknown options"

------------------------------------------------------------------------------------------------------------

table_main :: [String] -> IO ()
table_main ['-':'-':ns] | all isDigit ns && not (null ns) = do

        let mx = read ns

        -- Read what you can, please, into a Table.
        tab :: Table Row <- readTable stdin

--        print (HashMap.elems tab)
--        print (map HashMap.keys (HashMap.elems tab))
        
        let keys :: Set Text = Set.fromList $ pack "id" : concatMap HashMap.keys (HashMap.elems tab)
        
        let keyMx = id -- fmap (\ (k,v) -> (k,max (Text.length k) v))
                  $ sortBy (\ (k1,_) (k2,_) -> if k1 == k2 then EQ else
                                               if k1 == pack "id" then LT else
                                               if k2 == pack "id" then GT else
                                               compare k1 k2)
                  $ HashMap.toList
                  $ HashMap.fromListWith max $
                        [ (k',min mx $ length $ raw $ encode v') | (k,v) <- HashMap.toList tab, (k',v') <- HashMap.toList v ] ++
                        [ (pack "id",Text.length k) | (k,v) <- HashMap.toList tab] ++
                        [ (k,Text.length k) | k <- Set.toList keys ]


        let rjust txt n = take (n - length txt) (repeat ' ') ++ take n txt

--        putStrLn $ show [ (k,v) | (k,v) <- keyMx ]
        putStrLn $ unwords [ rjust (unpack k) v | (k,v) <- keyMx ]

        sequence_ [ putStrLn $ unwords [ case HashMap.lookup kk v' of
                                           Nothing -> rjust "" kv
                                           Just o -> rjust o kv 
                                       | (kk,kv) <- keyMx 
                                       ]
                  | (k,v) <- HashMap.toList tab
                  , let v' = HashMap.insert (pack "id") (unpack k) $ fmap (raw . encode) v
                  ]
table_main _ = error "crud table: unknown options"
        
------------------------------------------------------------------------------------------------------------
        
update_main :: [String] -> [String] -> IO ()
update_main opts [db] = case opts of
                         []        -> update False
                         ["--sub"] -> update True
  where  
    update isJoined = do
        let f new old | isJoined  = HashMap.union new old  -- join the two rows, use new over old if column matched
                      | otherwise = new                    -- simple replace with 2nd row
        new <- readTable stdin
        crud <- persistantCRUD db
        sequence_ [ do ans1 <- getRow crud id
                       case ans1 of
                         Nothing   -> updateRow crud (Named id row)
                         Just (Named _ row') -> updateRow crud (Named id (f row row'))
                  | (id,row :: Row) <- HashMap.toList new
                  ]
        return ()
update_main _ _ = error "crud update: unknown options"

------------------------------------------------------------------------------------------------------------

server_main :: [String] -> [String] -> IO ()
server_main flags (port:dbs) | all isDigit port && not (null port) = scotty (read port) $ do
  sequence_ [ do crud <- liftIO $ persistantCRUD db
                 scottyCRUD ('/':db) (crud :: CRUD Row)
            | db <- dbs 
            ]

server_main _ _ = error "crud server: unknown options"


------------------------------------------------------------------------------------------------------------

-- Get the raw ASCII text, please
raw :: BS.ByteString -> String
raw = map (chr.fromIntegral) . BS.unpack        
        