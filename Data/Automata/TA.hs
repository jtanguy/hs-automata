{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Automata.TA where

import Development.Placeholders
import Data.Char
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

mkLocation :: String -> Q [Dec]
mkLocation s = fmap (:[]) $ dataD (cxt []) (mkName "Location") [] locs [''Show, ''Eq]
      --  locs :: [Con]
    where locs = map (\(l:ls) -> normalC (mkName (toUpper l:ls)) []) $ words s

mkAction :: String -> Q [Dec]
mkAction s = fmap (:[]) $ dataD (cxt []) (mkName "Action") [] acts [''Show, ''Eq]
      --  acts :: [Con]
    where acts = map (\(l:ls) -> normalC (mkName (toUpper l:ls)) []) $ words s

mkClock :: String -> Q [Dec]
mkClock s = fmap (:[]) $ dataD (cxt []) (mkName "Clock") [plainTV (mkName "a")] clks [''Show, ''Eq, ''Functor, ''Typeable]
      --  clks :: [Con]
   where clks = map mkClkDataConst $ words s
         mkClkDataConst =  (\(l:ls) -> recC (mkName (toUpper l:ls)) [varStrictType (mkName "getValue") (strictType notStrict (varT (mkName "a")) )]) 
