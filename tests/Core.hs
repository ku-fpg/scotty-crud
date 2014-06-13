{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeOperators, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs #-}

module Main where

import qualified Data.Text as Text
import Data.Text(Text,pack)
import Web.CRUD
import System.IO
import System.Directory
import Control.Applicative
import Control.Monad
import Control.Lens ((^.))
import qualified Data.HashMap.Strict as HashMap
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.QuickCheck.Function
--TMP
import Control.Concurrent (threadDelay)

import Data.Aeson

import QC

slowCheck = quickCheckWith stdArgs { maxSuccess = 1000 }

main = slowCheck prop_crud

-- Simple tests
-- Saving, then loading again, will get back to the same CRUD.

data CRUDAction row a where
   -- CRUD
   CreateRow :: row -> CRUDAction row () 
   GetRow    :: Id  -> CRUDAction row ()       -- auto-compared
   UpdateRow :: Named row -> CRUDAction row ()
   DeleteRow :: Id        -> CRUDAction row ()
   -- Hack to get round the function QC function issue (TODO: revisit)
   -- Gets a stored ID, based on float between [0..1).
   GetId     :: Float -> CRUDAction row Text
   -- External
   Restart   :: CRUDAction row ()
   -- Assert
   Assert    :: Bool -> String -> CRUDAction row ()
   -- Monad
   Bind      :: CRUDAction row a -> (a -> CRUDAction row b) -> CRUDAction row b
   Return    :: a -> CRUDAction row a

instance Monad (CRUDAction row) where
        (>>=) = Bind
        return = Return

instance Applicative (CRUDAction row) where
        (<*>) = liftM2 ($)
        pure = Return
        
instance Functor (CRUDAction row) where
        fmap f act = pure f <*> act

instance Show row => Show (CRUDAction row a) where
        show (CreateRow row) = "CreateRow {..}" -- ++ show row
        show (UpdateRow row) = "UpdateRow {..}" -- ++ show row
        show (DeleteRow id)  = "DeleteRow " ++ show id
        show (GetRow iD)     = "GetRow " ++ show iD
        show (GetId n)       = "GetId " ++ show n
        show (Restart)       = "Restart "
        
        show (Bind m@(CreateRow {}) k) = show m ++ ";\n" ++ show (k ())
        show (Bind m@(UpdateRow {}) k) = show m ++ ";\n" ++ show (k ())
        show (Bind m@(DeleteRow {}) k) = show m ++ ";\n" ++ show (k ())
        show (Bind m@(GetRow {}) k)    = show m ++ ";\n" ++ show (k ())
        show (Bind m@(GetId n) k)      = show m ++ ";\n" ++ show (k (pack $ "<id-" ++ show n ++ ">"))
        show (Bind m@(Restart {}) k)   = show m ++ ";\n" ++ show (k ())
        show (Return _) = "Return"

data Env row = Env 
        { theCRUD  :: CRUD IO row
        , fileName :: FilePath
        , handle   :: Handle
        , ids      :: [Text]
        , oracle   :: [(Text,Named row)]
        , debug    :: IO () -> IO ()
--        , restart  :: 
        }

interpBind :: (ToJSON row, FromJSON row, Show row, Eq row) => CRUDAction row a -> (a -> CRUDAction row b) ->  Env row -> IO Bool
interpBind (GetRow iD) k env = do
        debug env $ putStrLn $ "GetRow " ++ show iD
        ans <- getRow (theCRUD env) iD
        case (ans,lookup iD (oracle env)) of
          (a1,a2) | a1 == a2 -> interp (k ()) env
          _ -> return False
interpBind (CreateRow row) k env = do
        debug env $  putStrLn $ "CreateRow {}" -- ++ show row
        Named iD' row' <- createRow (theCRUD env) row
        let env' = env { oracle = (iD',Named iD' row') : [ (k,v) | (k,v) <- oracle env, k /= iD' ] 
                       , ids = ids env ++ [iD']
                       }
        interp (k ()) env'
interpBind (UpdateRow (Named iD row)) k env = do
        debug env $ putStrLn $ "UpdateRow {}" -- ++ show (Named iD row)
        updateRow (theCRUD env) (Named iD row)
        let env' = env { oracle = (iD,Named iD row) : [ (k,v) | (k,v) <- oracle env, k /= iD ] 
                       , ids = ids env ++ if iD `elem` (ids env) then [] else [iD]
                       }
        interp (k ()) env'
interpBind (DeleteRow iD) k env = do
        debug env $ putStrLn $ "DeleteRow {}" -- ++ show (Named iD row)
        deleteRow (theCRUD env) iD
        let env' = env { oracle = [ (k,v) | (k,v) <- oracle env, k /= iD ] 
                       }
        interp (k ()) env'
interpBind (GetId r) k env = interp (k (xs !! n)) env
   where 
         n  = floor (r * fromIntegral (length xs))
         xs = iD : ids env   
         iD = head [ t :: Text
                   | n <- [1..] :: [Int]
                   , let t = Text.pack (show n)
                   , not (t `elem` (ids env))
                   ]
interpBind (Restart) k env = do
        interp (k ()) env 
{-
        debug env $ putStrLn "Restart"
        shutdown (theCRUD env) "restart"   -- wait until it is all done
--        sync (theCRUD env) 
	let loop = do
		b <- hIsClosed (handle env)
		if b then return () else do
		        threadDelay (10 * 1000)
			loop
        loop
        -- Flush the buffer
--        hFlush (handle env)
--        hClose (handle env)
--        threadDelay (2 * 1000 * 1000)
        h <- openBinaryFile test_json ReadWriteMode
        crud <- openCRUD h
        interp (k ()) (env { handle = h, theCRUD = atomicCRUD crud })
-}

interpBind (Assert b msg) k env = do
        if b
        then interp (k ()) env
        else do putStrLn $ "Assert fail" ++ msg
                return False
        
interpBind (Return a) k env = interp (k a) env
interpBind (Bind m k2) k env = interpBind m (\ r -> Bind (k2 r) k) env
interpBind other k env = error $ "interpBind: " ++ show other

interp :: (ToJSON row, FromJSON row, Show row, Eq row) => CRUDAction row a ->  Env row -> IO Bool
interp (Bind m k) env = interpBind m k env
interp (Return _) env = do
        -- Does not breaking monad laws here, because this is the *final* return only.
        shutdown (theCRUD env) "done"
        return True
interp other      env = interpBind other Return env

test_json = "tmp/test.json" :: String


runCRUDAction :: CRUDAction Object () -> IO Bool
runCRUDAction prog = do
        -- First, clear the start
        b <- doesFileExist test_json
        if b
        then removeFile test_json
        else return ()

        crud <- persistantCRUD test_json
        let debugging _ = return ()
        ans <- interp prog $ Env (atomicCRUD crud) test_json (error "h") [] [] debugging 
        return ans      

prop_crud :: Property
prop_crud = monadicIO $ do
        code <- pick (generateTest 10 0)
        ans <- run $ runCRUDAction code
        assert ans
        return ()      


act :: CRUDAction Object ()
act = do
      CreateRow (HashMap.fromList [("x", Number 1),("y", Number 2)])  
      id1 <- GetId 1
      GetRow id1
      GetRow "undefined"
      CreateRow (HashMap.fromList [("x", Number 1),("y", Number 2)])  
      id2 <- GetId 2
      Assert (id1 /= id2) "ids from create should be unique"


{-
         
generateID :: Gen ([Text] :-> Text)
generateID = do
        i <- choose (0,101)
        return $ function $ \ xs -> 
            cycle (head [ t
                        | n <- [1..]
                        , let t = Text.pack (show n)
                        , not (t `elem` xs)
                        ] : xs) !! i
-}

generateTest :: Int -> Int -> Gen (CRUDAction Object ())
generateTest n idCount = frequency 
        [ ( if n > 0 then 10 else 0
          , do row <- arbitraryHashMap False 5
               rest <- generateTest (n-1) (idCount + 1)
               return $ CreateRow row >>= \ () -> rest
          )
        , ( if n > 0 then 20 else 0
          , do r <- choose (0,0.9999)
               rest <- generateTest (n-1) idCount
               return $ GetId r >>= \ iD -> GetRow iD >>= \ () -> rest
          )
        , ( if n > 0 then 5 else 0
          , do rest <- generateTest (n-1) idCount
               return $ Restart >>= \ () -> rest
          )
        , ( if n > 0 then 10 else 0
          , do r <- choose (0,0.9999)
               row <- arbitraryHashMap False 5
               rest <- generateTest (n-1) idCount
               return $ GetId r >>= \ iD -> UpdateRow (Named iD row) >>= \ () -> rest
          )
        , ( if n > 0 then 10 else 0
          , do r <- choose (0,0.9999)
               rest <- generateTest (n-1) idCount
               return $ GetId r >>= \ iD -> DeleteRow iD >>= \ () -> rest
          )
        , (1,return (return ()))
        ]

