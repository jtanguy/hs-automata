
module Data.Automata.DFA (
  DFA
, mkDFA
, walk
, accept
, isComplete
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data DFA = DFA {
                locations     :: ( Set Loc )    -- A finite set of locations
               ,alphabet      :: ( Set Action ) -- An alphabet is a set of actions
               ,transitions   :: ( TrMap )      -- A transition function
               ,initial       :: ( Loc )        -- The initial location
               ,terminal      :: ( Set Loc )    -- Accepting locations
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
                                  , terminal = Set.fromList terms
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
accept dfa word = Set.member (evalState (walk word) dfa) (terminal dfa)

isComplete :: DFA -> Bool
isComplete dfa = all (\k -> Map.member k (transitions dfa)) [(l,a) | l <- Set.elems $locations dfa, a <- Set.elems $ alphabet dfa]
