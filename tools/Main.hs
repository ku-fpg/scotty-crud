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

main :: IO ()
main = getArgs >>= main2

main2 :: [String] -> IO ()
main2 ["compress"] = do
        tab :: Table Row <- readTable stdin
        writeTable stdout tab
main2 ["compress",db] = do
        h <- openBinaryFile db ReadMode
        tab :: Table Row <- readTable h
        hClose h
        h <- openBinaryFile db WriteMode
        writeTable h tab
        hClose h
main2 ["table"] = table_main 20
main2 ['-':'-':ns,"table"] | all isDigit ns && not (null ns) = table_main (read ns)
main2 [         "update",db] = update_main False db
main2 ["--join","update",db] = update_main True db

main2 _ = error $ unlines
                [ "usage: crud [options] [command] [files]"
                , "         where command = compress | table | update"
                , ""
                , "  crud compress < input.json > compressed-output.json"
                , "  crud compress db.json"
                , ""
                , "  crud [--'int'] table < input.json | less"
                , ""
                , "  crud [--join] update db.json < new.json"
                ]

-- Get the raw ASCII text, please
raw :: BS.ByteString -> String
raw = map (chr.fromIntegral) . BS.unpack        

table_main :: Int -> IO ()
table_main mx = do

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
        
        
update_main :: Bool -> String -> IO ()
update_main isJoined db = do
        let f new old | isJoined  = HashMap.union new old  -- join the two maps, use new over old if matched
                      | otherwise = new                   -- simple repalce
        new <- readTable stdin
        crud <- persistantCRUD db
        sequence_ [ do ans1 <- getRow crud id
                       case ans1 of
                         Nothing   -> updateRow crud (Named id row)
                         Just (Named _ row') -> updateRow crud (Named id (f row row'))
                  | (id,row :: Row) <- HashMap.toList new
                  ]
        return ()

        