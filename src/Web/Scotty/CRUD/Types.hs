{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Web.Scotty.CRUD.Types (
       CRUD(..),
       Id, Table, Row, Named(..),
       namedRowToRow, rowToNamedRow, lookupColumn
       ) where

import           Control.Applicative

import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import           Data.Text(Text)
import qualified Data.HashMap.Strict as HashMap

------------------------------------------------------------------------------------
-- | A CRUD is a OO-style database Table of typed rows, with getters and setters.
--   The default row is a JSON Object.
data CRUD row = CRUD
     { createRow :: row       -> IO (Named row)
     , getRow    :: Id        -> IO (Maybe (Named row))
     , getTable  ::              IO (Table row)
     , updateRow :: Named row -> IO ()
     , deleteRow :: Id        -> IO () -- alway works
     }
------------------------------------------------------------------------------------
-- Basic synonyms for key structures
--
-- | Every (Named) Row must have an id field.
type Id        = Text

-- | a Table is a HashMap of Ids to rows, typically 'Table Row'.
--   The elems of the Table do not contain, by default, the Id, because this
--   is how you index a row. Note that the output of a complete table,
--   via RESTful CRUD, injects the Id into the row.

type Table row = HashMap Id row

-- | The default row is a aeson JSON object.
type Row       = Object

------------------------------------------------------------------------------------
-- | A pair of Name(Id) and row.
data Named row = Named Id row
   deriving (Eq,Show)

instance FromJSON row => FromJSON (Named row) where
    parseJSON (Object v) = Named
                <$> v .: "id"
                <*> (parseJSON $ Object $ HashMap.delete "id" v)
    parseJSON _ = fail "row should be an object"

instance ToJSON row => ToJSON (Named row) where
   toJSON = Object . namedRowToRow


namedRowToRow :: (ToJSON row) => Named row -> Row
namedRowToRow (Named key row) = case toJSON row of
     Object env -> HashMap.insert "id" (String key) env
     _ -> error "row should be an object"


rowToNamedRow :: FromJSON row => Row -> Named row
rowToNamedRow row = case fromJSON (Object row) of
     Error msg -> error $ msg
     Success a -> a

lookupColumn ::ToJSON row => Text -> row -> Maybe Value
lookupColumn nm row = case toJSON row of
     Object env -> HashMap.lookup nm env
     _ -> error "row should be an object"
