{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Data.Function (on)
import Data.Typeable
import Development.Placeholders

-- Definition of a TA

type Arc = Location -> Action -> (Constraint, [Clock], Location)

type Invariant = Location -> Constraint

data Constraint = Single Clock Op Double | Diag Clock Clock Op Double | And Constraint Constraint

instance Show Constraint where
    show (Single c op i) = show c ++ show op ++ show i
    show (Diag c1 c2 op i) = show c1 ++ "-" ++ show c2 ++ show op ++ show i
    show (And g1 g2) = show g1 ++ "^" ++ show g2

data Op = Lt | Lte | E | Gte | Gt 

instance Show Op where
    show Lt = "<"
    show Lte= "<="
    show E = "="
    show Gte = ">="
    show Gt = ">"

-- *Declaration of the TA
data Location = R0 | R1 | R2 deriving (Show,Eq, Enum)
--
-- Initial Location
l0 :: Location
l0 = R0

data Clock = X deriving (Show,Eq,Enum)

data Action = A | B | C deriving (Show,Eq,Enum)

(-->) :: Arc
(-->) R0 A = (Single X Gte 3,[X],R1)
(-->) R1 B = (Single X E 2,[],R0)
(-->) R0 C = (Single X E 2.4,[],R2)
(-->) R2 A = (Single X Gte 4,[X],R0)

inv :: Invariant
inv R1 = Single X Lte 2
inv R2 = Single X Lte 5
