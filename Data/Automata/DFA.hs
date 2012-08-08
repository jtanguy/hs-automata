{-# LANGUAGE OverloadedStrings #-}
-- |The module Data.Automata.DFA provides Deterministic Finite Automata
-- type and methods to create, walk, test whether it accepts a word,
-- and check for completeness
--
-- *TODO
-- - Validate a DFA, by checking if the set of finite states is a subset of the set of states
module Data.Automata.DFA (
  DFA
, mkDFA
, walk
, accept
, isComplete
, dfa2dot
) where

import Control.Monad.State
import Data.GraphViz
import Data.GraphViz.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data DFA = DFA {
                locations     :: ( Set Loc )    -- A finite set of locations
               ,alphabet      :: ( Set Action ) -- An alphabet is a set of actions
               ,transitions   :: ( TrMap )      -- A transition function
               ,initial       :: ( Loc )        -- The initial location
               ,terminals      :: ( Set Loc )    -- Accepting locations
               }

type Loc = String
type Action = Char
type TrMap = Map (Loc, Action) Loc

mkDFA :: [Loc] -> [Action] -> [((Loc,Action),Loc)] -> Loc -> [Loc] -> DFA
mkDFA locs alph trs iniLoc terms = DFA {
                                    locations = Set.fromList locs
                                  , alphabet = Set.fromList alph
                                  , transitions = Map.fromList trs
                                  , initial = iniLoc
                                  , terminals = Set.fromList terms
                                  }


walk :: String -> State DFA Loc
walk [] = do
            dfa <- get
            return $ initial dfa
walk (a:as) = do
            dfa <- get
            put dfa { initial = transitions  dfa Map.! (initial dfa, a) }
            walk as

accept :: DFA -> [Action] -> Bool
accept dfa word = Set.member (evalState (walk word) dfa) (terminals dfa)

isComplete :: DFA -> Bool
isComplete dfa = all (\k -> Map.member k (transitions dfa)) [(l,a) | l <- Set.elems $locations dfa, a <- Set.elems $ alphabet dfa]


dfa2dot :: DFA -> DotGraph String
dfa2dot dfa = DotGraph  { strictGraph = False
                        , directedGraph = True
                        , graphID = Nothing
                        , graphStatements = DotStmts    { attrStmts = [NodeAttrs [shape Circle]]
                                                        , subGraphs = []
                                                        , nodeStmts = (  [ DotNode "init" [shape PlainText, textLabel ""]
                                                                        ]
                                                                      ++ map (\n->DotNode n []) (Set.toList ((locations dfa) Set.\\ (terminals dfa)))
                                                                      ++ map (\n->DotNode n [shape DoubleCircle]) (Set.toList (terminals dfa))
                                                                      )
                                                        , edgeStmts = ([DotEdge "init" (initial dfa) []]
                                                                      ++ (map (\((n1,a),n2)->DotEdge n1 n2 [toLabel a]) $ Map.toList (transitions dfa)
                                                                      ))
                                                        }
                        }
