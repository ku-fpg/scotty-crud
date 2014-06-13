module Web.CRUD.Scotty where

import Data.Aeson
import Web.Scotty
import qualified Data.HashMap.Strict as HashMap

import Web.CRUD

-- | scottyCRUD provides scotty support for a CRUD object.
-- 
-- > crud <- liftIO $ persistantCRUD "filename"
-- > scottyCRUD "foo" (atomicCRUD crud)

scottyCRUD :: (FromJSON row, ToJSON row) => String -> CRUD IO row -> ScottyM ()
scottyCRUD url crud = return ()
