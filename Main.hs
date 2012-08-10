{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Automata.TA
import Data.Function (on)
import Data.Typeable
import Development.Placeholders



$(mkLocation "Q0 Q1") 

initial :: Location
initial = Q0

$(mkAction "a b c")

$(mkClock "X Y")

eq :: Num a => Clock a -> Clock a -> Bool
eq = (==) `on` fmap (*0)

type Valuation = [Clock Int]

-- | TODO: derive this in TH
zero :: Valuation
zero = [X 0,Y 0]

plusT :: Valuation -> Int -> Valuation
plusT v t = map (fmap (+t)) v

withR :: Valuation -> [Clock a] -> Valuation
withR v cs = fmap resetClock v
    where resetClock  = $notImplemented

