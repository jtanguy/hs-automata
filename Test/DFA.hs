
module Test.DFA where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Automata.DFA


dfa :: DFA
dfa = mkDFA  [ "q0","q1","q2"]
             [ 'a','b']
             [
               (("q0",'a'), "q1")
             , (("q1",'a'), "q1")
             , (("q1",'b'), "q2")
             , (("q2",'b'), "q2")
             ]
             "q0"
             ["q2"]

