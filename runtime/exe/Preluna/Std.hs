{-# LANGUAGE OverloadedStrings #-}

module Preluna.Std where

import Prologue

import qualified Data.Map      as Map
import qualified OCI.Data.Name as Name
import qualified Luna.Runtime  as Luna
import Data.Map (Map)

type BaseModule = "Std.Base"
baseModule :: Name.Qualified
baseModule = Name.qualFromSymbol @BaseModule

type instance Luna.RuntimeRepOf Bool = Luna.AsClass Bool ('Luna.ClassRep BaseModule "Bool")
instance Luna.FromObject Bool where
    fromConstructor c = return $ c ^. Luna.tag == "True"
instance Luna.ToObject Bool where
    toConstructor _ b = Luna.Constructor (if b then "True" else "False") []

{-dummyMethMap = Map.fromList $ flip fmap [1..100] $ \i -> (convert $ show i, undefined)-}

methodMaps :: Map Name.Name Luna.Class
methodMaps = let cls = Luna.Class def in Map.fromList $ (,cls) <$> ["Bool", "Text", "Int", "None"]

glob0 :: Luna.Units
glob0 = Luna.Units $ Map.singleton baseModule unit where
    unit = Luna.Unit def methodMaps

std :: Luna.Units
std = glob0 & wrapped . ix baseModule . Luna.defs .~ fmap pure (Map.fromList stds) where
    stds = [ ("putStr", Luna.toValue glob0 (putStrLn . convert :: Text -> IO ()))
           , ("if.then.else", Luna.toValue glob0 ((\a b c -> if a then b else c) :: Bool -> Luna.Value -> Luna.Value -> Luna.Value))
           , ("intPlus", Luna.toValue glob0 ((+) :: Integer -> Integer -> Integer))
           , ("intMinus", Luna.toValue glob0 ((-) :: Integer -> Integer -> Integer))
           , ("intLt",   Luna.toValue glob0 ((<) :: Integer -> Integer -> Bool))
           , ("intShow", Luna.toValue glob0 (convert . show :: Integer -> Text))
           ]
