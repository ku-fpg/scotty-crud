{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
module Web.Scotty.CRUD.SQL (
         -- * SQL-style SELECT, and other operators
        sqlSelect, sqlSortBy, sqlWhere, like,
        SortKey(..)
         ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text

import           Web.Scotty.CRUD.Types

------------------------------------------------------------------------------------

sqlSelect :: [Text] -> [Row] -> [Row]
sqlSelect ns rows = fmap (\ row -> HashMap.fromList [ (k,v) | (k,v) <- HashMap.toList row, k `elem` ns]) rows


sqlSortBy :: Text -> [Row] -> [Row]
sqlSortBy nm rows = map snd $ sortBy cmp $ prep
          where
                 cmp  a b = fst a `compare` fst b

                 key :: Row -> Maybe SortKey
                 key row = fmap SortKey $ HashMap.lookup nm row

                 prep :: [(Maybe SortKey,Row)]
                 prep = map (\ v -> (key v,v)) rows



sqlWhere :: Text -> (SortKey -> Bool) -> Table Row -> Table Row
sqlWhere nm op tab = liftTable (filter f) tab
  where
      f row = case HashMap.lookup nm row of
               Just value -> op (SortKey value)
               Nothing -> False

------------------------------------------------------------------------------------

liftTable :: ([Row] -> [Row]) -> Table Row -> Table Row
liftTable f = HashMap.fromList
            . map (\ (Named key row) -> (key,row))
            . map rowToNamedRow
            . f
            . map namedRowToRow
            . map (\ (key,row)-> Named key row)
            . HashMap.toList

------------------------------------------------------------------------------------

newtype SortKey = SortKey Value
  deriving (Eq,Show)

instance Ord SortKey where
  compare (SortKey (String s1)) (SortKey (String s2)) = s1 `compare` s2
  compare (SortKey (Number s1)) (SortKey (Number s2)) = s1 `compare` s2
  compare s1 s2 = show s1 `compare` show s2

instance Num SortKey where
  (+) = error "(+)"
  (-) = error "(-)"
  (*) = error "(*)"
  abs = error "abs"
  signum = error "signum"

  fromInteger = SortKey . Number . fromInteger

instance Fractional SortKey where
  recip = error "recip"
  fromRational = SortKey . Number . fromRational

instance IsString SortKey where
  fromString = SortKey . String . fromString


like :: SortKey -> Text -> Bool
like (SortKey (String s)) ms = case parsePercent ms of
    (False,txt,True ) -> txt `Text.isPrefixOf` s
    (True, txt,False) -> txt `Text.isSuffixOf` s
    (True, txt,True ) -> txt `Text.isInfixOf` s
    (False,txt,False) -> txt == s
like _ _ = False

parsePercent :: Text -> (Bool,Text,Bool)
parsePercent txt0 = (hd,txt2,tl)
  where
    (hd,txt1) = if not (Text.null txt0) && Text.head txt0 == '%'
                then (True,Text.tail txt0)
                else (False,txt0)

    (txt2,tl) = if not (Text.null txt1) && Text.last txt1 == '%'
                then (Text.init txt1,True)
                else (txt1,False)
