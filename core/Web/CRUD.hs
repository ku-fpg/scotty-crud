{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Web.CRUD (
       -- * Basic types
       CRUD(..),
       Id, Table, Row, Named(..),
       -- * CRUD functions
       atomicCRUD,
       openCRUD,
       persistantCRUD,
       createCRUD,
       readOnlyCRUD,
       -- * Table functions
       readTable,
       writeTable,
       -- * Table updates
       TableUpdate(..),
       tableUpdate,
       writeTableUpdate,
       ) where

import Data.Aeson
import Data.Aeson.Parser as P
import Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Applicative
import Data.Char (isSpace, isDigit, chr)
import Data.List (foldl')
import Data.Text (Text, pack)
import Control.Monad
import qualified Data.Text as Text
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import System.IO

------------------------------------------------------------------------------------
-- | A CRUD is a OO-style database Table of typed rows, with getters and setters. 
--   The default row is a JSON Object.
data CRUD m row = CRUD
     { createRow :: row       -> m (Named row)
     , getRow    :: Id        -> m (Maybe (Named row))
     , getTable               :: m (Table row)
     , updateRow :: Named row -> m ()
     , deleteRow :: Id        -> m () -- alway works
     , shutdown  :: Text      -> IO ()  -- waits until shutdown complete (and persistant file(s) written to disk)
     }

------------------------------------------------------------------------------------
-- Basic synonyms for key structures 
--
type Id        = Text
type Table row = HashMap Id row
type Row       = Object

------------------------------------------------------------------------------------
-- | A pair of Name(Id) and row.
data Named row = Named Id row
   deriving (Eq,Show)

instance FromJSON row => FromJSON (Named row) where
    parseJSON (Object v) = Named
                <$> v .: "id"
                <*> (parseJSON $ Object $ HashMap.delete "id" v)
                
instance ToJSON row => ToJSON (Named row) where                
   toJSON (Named key row) = 
                   case toJSON row of
                     Object env -> Object $ HashMap.insert "id" (String key) env
                     _ -> error "row should be an object"


------------------------------------------------------------------------------------
-- CRUD functions

-- | take a STM-based CRUD, and return a IO-based CRUD
atomicCRUD :: CRUD STM row -> CRUD IO row
atomicCRUD crud = CRUD 
     { createRow = atomically . createRow crud
     , getRow    = atomically . getRow crud
     , getTable  = atomically $ getTable crud
     , updateRow = atomically . updateRow crud 
     , deleteRow = atomically . deleteRow crud
     , shutdown  = shutdown crud
     }

-- | We store our CRUD in a simple format; a list of newline seperated
-- JSON objects, in the order they were applied, where later objects
-- subsumes earlier ones. If the Handle provided is ReadWrite,
-- the subsuquent updates are recorded after the initial ones.
-- There is no attempt a compaction; we only append to the file.
-- 
-- Be careful: the default overloading of () for FromJSON will not work.

openCRUD :: forall row . (Show row, ToJSON row, FromJSON row) => Handle -> IO (CRUD STM row)
openCRUD h = do

    env <- readTable h 

    -- This is our table,
    table <- newTVarIO env
    updateChan <- newTChanIO
    
    let top :: STM Integer
        top = do t <- readTVar table
                 return $ foldr max 0
                          [ read (Text.unpack k)
                          | k <- HashMap.keys t
                          , Text.all isDigit k
                          ]

    uniq <- atomically $ do
               mx <- top
               newTVar (mx + 1)

    -- Get the next, uniq id when creating a row in the table.
    let next :: STM Text           
        next = do
               n <- readTVar uniq
               let iD = Text.pack (show n) :: Text
               t <- readTVar table
               if HashMap.member iD t
               then do mx <- top
                       t <- writeTVar uniq (mx + 1)
                       next
                 -- Great, we can use this value
               else do writeTVar uniq $! (n + 1)
                       return iD

    let updateCRUD update = do
          modifyTVar table (tableUpdate update)
          writeTChan updateChan update

    let handler m = m `catches`
         []
{-
          [ {-Handler $ \ (ex :: SomeAsyncException) -> return ()
          , -}Handler $ \ (ex :: SomeException) -> do { print ("X",ex) ; return (); } 
                          -- print ("XX",ex) ; return () }
          ]
-}
    flushed <- newTVarIO True
    done <- newEmptyTMVarIO

    let loop = do
          tu <- atomically $ do
--                  writeTVar flushed False
                  readTChan updateChan
--          print $ "writing" ++ show tu
          LBS.hPutStr h (encode tu)
          LBS.hPutStr h "\n" -- just for prettyness, nothing else
          hFlush h
--          atomically $ writeTVar flushed True
          case tu of
             Shutdown {} -> do
                     hClose h
--                     atomically $ putTMVar done ()   -- final act
                     return ()
             _ -> loop

    forkIO $ handler $ loop

    let syncCRUD = do
            flush_status <- readTVar flushed
            chan_status <- isEmptyTChan updateChan
            check (flush_status && chan_status)

    return $ CRUD
     { createRow = \ row    -> do iD <- next
                                  let row' = Named iD row
                                  updateCRUD (RowUpdate row')
                                  return row'
     , getRow    = \ iD     -> do t <- readTVar table
                                  return $ fmap (Named iD) $ HashMap.lookup iD t
     , getTable  =             do readTVar table
     , updateRow = updateCRUD . RowUpdate 
     , deleteRow = updateCRUD . RowDelete
     , shutdown = \ msg -> do
                     atomically $ do  -- request shutdown
                             updateCRUD (Shutdown msg)
--                     atomically $ do  -- wait for shutdown      
--                             takeTMVar done
     }



actorCRUD :: (ToJSON row, FromJSON row) 
	 => (TableUpdate row -> STM ())	   
	 -> Table row	-- initial Table row
	 -> IO (CRUD STM row)
actorCRUD push env = do

    table <- newTVarIO env
    updateChan <- newTChanIO
    
    let top :: STM Integer
        top = do t <- readTVar table
                 return $ foldr max 0
                          [ read (Text.unpack k)
                          | k <- HashMap.keys t
                          , Text.all isDigit k
                          ]

    uniq <- atomically $ do
               mx <- top
               newTVar (mx + 1)

    -- Get the next, uniq id when creating a row in the table.
    let next :: STM Text           
        next = do
               n <- readTVar uniq
               let iD = Text.pack (show n) :: Text
               t <- readTVar table
               if HashMap.member iD t
               then do mx <- top
                       t <- writeTVar uniq (mx + 1)
                       next
                 -- Great, we can use this value
               else do writeTVar uniq $! (n + 1)
                       return iD

    let updateCRUD update = do
          modifyTVar table (tableUpdate update)
          push update

    let handler m = m `catches`
         []
{-
          [ {-Handler $ \ (ex :: SomeAsyncException) -> return ()
          , -}Handler $ \ (ex :: SomeException) -> do { print ("X",ex) ; return (); } 
                          -- print ("XX",ex) ; return () }
          ]
-}
    flushed <- newTVarIO True
    done <- newEmptyTMVarIO

    return $ CRUD
     { createRow = \ row    -> do iD <- next
                                  let row' = Named iD row
                                  updateCRUD (RowUpdate row')
                                  return row'
     , getRow    = \ iD     -> do t <- readTVar table
                                  return $ fmap (Named iD) $ HashMap.lookup iD t
     , getTable  =             do readTVar table
     , updateRow = updateCRUD . RowUpdate 
     , deleteRow = updateCRUD . RowDelete
     , shutdown = \ msg -> do
                     atomically $ do  -- request shutdown
                             updateCRUD (Shutdown msg)
     }

-- | We store our CRUD in a simple format; a list of newline seperated
-- JSON objects, in the order they were applied, where later objects
-- subsumes earlier ones. If the Handle provided is ReadWrite,
-- the subsuquent updates are recorded after the initial ones.
-- There is no attempt a compaction; we only append to the file.
-- 
-- Be careful: the default overloading of () for FromJSON will not work.

persistantCRUD :: (FromJSON row, ToJSON row) => FilePath -> IO (CRUD STM row)
persistantCRUD fileName = do
        h <- openBinaryFile fileName ReadWriteMode
        -- Read what you can, please, into a Table.
        tab <- readTable h 

        -- check for EOF & writeable, etc

        -- Now, write any changes after what you have read, in the same file
        push <- writeableTableUpdate h

        -- Finally, set of the CRUD object
        actorCRUD push tab


-- | create a CRUD without a backing store, based on an existing Table.

createCRUD :: (FromJSON row, ToJSON row) => Table row -> IO (CRUD STM row)
createCRUD = error ""

-- | create a CRUD that does not honor write requests.
-- This will call 'fail' for any attempted writes.
readOnlyCRUD :: (Monad m) => CRUD m row -> CRUD m row
readOnlyCRUD crud = CRUD 
     { createRow = \ iD  -> fail "read only / createRow"
     , getRow    = \ iD     -> getRow crud iD
     , getTable  =             getTable crud
     , updateRow = \ row -> fail "read only / updateRow"
     , deleteRow = \ iD  -> fail "read only / deleteRow"
     , shutdown =  shutdown crud
     }


------------------------------------------------------------------------------------
-- Table

readTable :: (FromJSON row) => Handle -> IO (Table row)
readTable h = do

    let sz = 32 * 1024 :: Int

    let loadCRUD bs env
          | BS.null bs = do
                  bs' <- BS.hGet h sz
                  if BS.null bs'
                  then return env        -- done, done, done (EOF)
                  else loadCRUD bs' env
          | otherwise =
                  parseCRUD (Atto.parse P.json bs) env
        parseCRUD (Fail bs _ msg) env
                | BS.all (isSpace . chr . fromIntegral) bs = loadCRUD BS.empty env
                | otherwise = fail $ "parse error: " ++ msg
        parseCRUD (Partial k) env = do
                  bs <- BS.hGet h sz    
                  parseCRUD (k bs) env
        parseCRUD (Done bs r) env = do
                  case fromJSON r of
                    Error msg -> error msg
                    Success update -> loadCRUD bs $! tableUpdate update env

    loadCRUD BS.empty HashMap.empty 


writeTableUpdate :: (ToJSON row) => Handle -> TableUpdate row -> IO ()
writeTableUpdate h row = do
        LBS.hPutStr h (encode row)
        LBS.hPutStr h "\n" -- just for prettyness, nothing else
                     
writeTable :: (ToJSON row) => Handle -> Table row -> IO ()
writeTable h table = sequence_
        [ writeTableUpdate h $ RowUpdate (Named iD row)
        | (iD,row) <- HashMap.toList table
        ]

-- TODO: what happens if the TableUpdate contains _|_?
-- Perhaps there should be a deepseq requrement on the argument?
writeableTableUpdate :: (ToJSON row) => Handle -> IO (TableUpdate row -> STM ())
writeableTableUpdate h = do
    updateChan <- newTChanIO

    let loop = do
          tu <- atomically $ do
                  readTChan updateChan
--          print $ "writing" ++ show tu
          LBS.hPutStr h (encode tu)
          LBS.hPutStr h "\n" -- just for prettyness, nothing else
          hFlush h
          case tu of
             Shutdown {} -> do
                     hClose h
                     return ()
             _ -> loop

    forkIO $ loop
    
    return $ writeTChan updateChan

-- Changes all all either an update (create a new field if needed) or a delete.

data TableUpdate row
        = RowUpdate (Named row)
        | RowDelete Id
        | Shutdown Text       -- last message; please stop listening. Msg for informational purposes ony.
        deriving (Show, Eq)
        
instance ToJSON row => ToJSON (TableUpdate row) where
   -- Assumption: the obj contains an "id" key
   toJSON (RowUpdate namedRow) = toJSON namedRow
   toJSON (RowDelete key)      = Object $ HashMap.fromList [("delete",String key)]
   toJSON (Shutdown msg)       = Object $ HashMap.fromList [("shutdown",String msg)]

instance FromJSON row => FromJSON (TableUpdate row) where
    parseJSON (Object v) = 
        ( RowUpdate <$> parseJSON (Object v)) <|> 
        ( RowDelete <$> v .: "delete")        <|>
        ( Shutdown  <$> v .: "shutdown")

tableUpdate :: TableUpdate row -> Table row -> Table row
tableUpdate (RowUpdate (Named key row)) = HashMap.insert key row
tableUpdate (RowDelete key)             = HashMap.delete key
tableUpdate (Shutdown msg)              = id

