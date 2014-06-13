module Web.Scotty.CRUD where

import Data.Aeson
import Web.Scotty
import qualified Data.HashMap.Strict as HashMap

import Web.CRUD

-- | scottyCRUD provides scotty support for a CRUD object.
-- 
-- > crud <- liftIO $ persistantCRUD "filename"
-- > scottyCRUD "URL" (atomicCRUD crud)

scottyCRUD :: (FromJSON row, ToJSON row) => String -> CRUD IO row -> ScottyM ()
scottyCRUD url crud = do

        get (capture url) $ do 
                tab <- getTable crud 
                liftIO $ print tab        
                return ()
        
--        get (capture $ url ++ "/:id") $ do return ()

                

