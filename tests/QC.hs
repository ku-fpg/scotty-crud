{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module QC where

import Data.Aeson as A    
import qualified Data.Vector as V  
import Control.Applicative
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Hashable (Hashable)
import Control.Monad
import Data.Text (Text)
import Data.Scientific

import Test.QuickCheck
import Test.QuickCheck.Function

instance Arbitrary Text where
  arbitrary = do i <- choose (1,10)
                 Text.pack <$> vectorOf i (elements (['A'..'Z'] ++ ['a'..'z']))

newtype Label = Label Text
        deriving Show


-- Label lbl is never "id"        
instance Arbitrary Label where
  arbitrary = do lab <- arbitrary
                 if lab == "id"
                 then arbitrary
                 else return $ Label lab

instance Arbitrary Object where
        arbitrary = do
                i <- choose (1,10)
                b <- arbitrary
                arbitraryHashMap b i

-- The first argument is if we have an "id" or not, in this HashMap.
arbitraryHashMap :: Bool -> Int -> Gen Object
arbitraryHashMap b n = do 
        i <- choose (1,10)
        v <- arbitraryValue (n-1)
        ixVals <- vectorOf i (liftM2 (,) arbitrary (arbitraryValue (n-1)))
        return $ (HashMap.fromList . map (\(Label lbl,e) -> (lbl,e)))
                     ((if b then [(Label "id",v)] else []) ++ ixVals)


instance Arbitrary A.Array where
  arbitrary = do
          i <- choose (1,10)
          arbitraryArray i

arbitraryArray n = do 
        i <- choose (0,10)
        V.fromList <$> vectorOf i (arbitraryValue (n-1) :: Gen A.Value)

instance Arbitrary A.Value where
  arbitrary = do
          i <- choose (0,10)
          arbitraryValue i

arbitraryValue :: Int -> Gen Value
arbitraryValue n = frequency $ 
      [ (10,A.String <$> (arbitrary :: Gen Text))
      , (10,(Number . fromFloatDigits) <$> (arbitrary :: Gen Double))
      , (2,A.Bool   <$> (arbitrary :: Gen Bool))
      , (1,return A.Null)
      , ( if n >= 1 then 10 else 0
        , A.Object <$> (arbitraryHashMap False (n-1) :: Gen Object)
        )
      , ( if n >= 1 then 5 else 0
        , A.Array  <$> (arbitraryArray (n-1) :: Gen Array)
        )
      ]
