{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeOperators, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs #-}

module Main where

import           Control.Applicative
--TMP
import           Control.Concurrent (threadDelay)
import           Control.Monad

import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import           Data.Text (Text, pack)

import           QC

import           System.Directory
import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import           Web.Scotty.CRUD
import           Web.Scotty.CRUD.JSON

main :: IO ()
main = do
        createDirectoryIfMissing True "test-tmp"
        quickCheck (prop_crud PersistantCRUD)   -- Done not close handle, so will leak handles when tested.
        quickCheck (prop_crud RestartingCRUD)

slowCheck :: Testable prop => prop -> IO ()
slowCheck = quickCheckWith stdArgs { maxSuccess = 1000 }

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
        show (CreateRow _) = "CreateRow {..}" -- ++ show row
        show (UpdateRow _) = "UpdateRow {..}" -- ++ show row
        show (DeleteRow iD)  = "DeleteRow " ++ show iD
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
        { theCRUD  :: CRUD row
        , ids      :: [Text]
        , oracle   :: [(Text,Named row)]
        , debug    :: IO () -> IO ()
        , restart  :: Env row -> IO (Env row)
        , shutdown :: IO ()
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
        let env' = env { oracle = (iD',Named iD' row') : [ (k',v) | (k',v) <- oracle env, k' /= iD' ] 
                       , ids = ids env ++ [iD']
                       }
        interp (k ()) env'
interpBind (UpdateRow (Named iD row)) k env = do
        debug env $ putStrLn $ "UpdateRow {}" -- ++ show (Named iD row)
        updateRow (theCRUD env) (Named iD row)
        let env' = env { oracle = (iD,Named iD row) : [ (k',v) | (k',v) <- oracle env, k' /= iD ] 
                       , ids = ids env ++ if iD `elem` (ids env) then [] else [iD]
                       }
        interp (k ()) env'
interpBind (DeleteRow iD) k env = do
        debug env $ putStrLn $ "DeleteRow {}" -- ++ show (Named iD row)
        deleteRow (theCRUD env) iD
        let env' = env { oracle = [ (k',v) | (k',v) <- oracle env, k' /= iD ] 
                       }
        interp (k ()) env'
interpBind (GetId r) k env = interp (k (xs !! n)) env
   where 
         n  = floor (r * fromIntegral (length xs))
         xs = iD : ids env   
         iD = head [ t :: Text
                   | n' <- [1..] :: [Int]
                   , let t = Text.pack (show n')
                   , not (t `elem` (ids env))
                   ]
interpBind (Restart) k env = do
        debug env $ putStrLn "Restart"
        env' <- restart env env
        interp (k ()) env' 

interpBind (Assert b msg) k env = do
        if b
        then interp (k ()) env
        else do putStrLn $ "Assert fail" ++ msg
                return False
        
interpBind (Return a) k env = interp (k a) env
interpBind (Bind m k2) k env = interpBind m (\ r -> Bind (k2 r) k) env
interpBind other _ _ = error $ "interpBind: " ++ show other

interp :: (ToJSON row, FromJSON row, Show row, Eq row) => CRUDAction row a ->  Env row -> IO Bool
interp (Bind m k) env = interpBind m k env
interp (Return _) env = do
        -- Does not breaking monad laws here, because this is the *final* return only.
        shutdown env
        return True
interp other      env = interpBind other Return env

test_json :: String
test_json = "test-tmp/test.json" :: String

data CRUD_TEST_TYPE 
        = PersistantCRUD        -- loading once, using the persistentCRUD function
        | RestartingCRUD        -- loading many times.

runCRUDAction :: CRUD_TEST_TYPE -> CRUDAction Object () -> IO Bool
runCRUDAction PersistantCRUD prog = do
        -- First, clear the start
        b <- doesFileExist test_json
        if b
        then removeFile test_json
        else return ()

        crud <- persistentCRUD test_json
        let debugging _ = return ()
        let restart' env = return env
        let shutdown' = return ()
        interp prog $ Env crud [] [] debugging restart' shutdown'
        -- We should check that the env in the file is the same as the model
        
runCRUDAction RestartingCRUD prog = do
        -- First, clear the start
        b <- doesFileExist test_json
        if b
        then removeFile test_json
        else return ()

        -- new file
        h <- openBinaryFile test_json WriteMode
        
        -- Now, write any changes after what you have read, in the same file
        push <- writeableTableUpdate h
        
        -- Finally, set of the CRUD object
        crud <- actorCRUD push $ HashMap.empty

        let debugging _ = return ()
        let restart' h' push' env = do
                push' (Shutdown "restart")

                let loop = do
                        b' <- hIsClosed h'
                        if b'
                        then return () 
                        else do threadDelay (10 * 1000)
                                loop
                loop

                h'' <- openBinaryFile test_json ReadWriteMode
                tab <- readTable h''
                push'' <- writeableTableUpdate h''
                crud' <- actorCRUD push'' tab
                let env' = env { theCRUD = crud', restart = restart' h'' push'', shutdown = shutdown' push }
                return env'

            shutdown' push' = push' (Shutdown "bye")
        interp prog $ Env crud [] [] debugging (restart' h push) (shutdown' push)




prop_crud :: CRUD_TEST_TYPE -> Property
prop_crud ty = monadicIO $ do
        code <- pick (generateTest 10 0)
        ans <- run $ runCRUDAction ty code
        assert ans
        return ()      


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

