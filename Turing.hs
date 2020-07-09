module Turing where

import Data.Finite
import qualified Data.Map.Strict as Map

minus = (-)

data Motion = L | N | R

move :: Motion -> Integer -> Integer
move L = (minus 1)
move N = id
move R = (+ 1)

type Update symbolcount statecount =
    (Finite symbolcount, Finite statecount, Motion)

type Detect symbolcount statecount =
    (Finite symbolcount, Finite statecount)

type Status symbolcount statecount =
    (Finite statecount, Integer, Tape symbolcount)

type Machine symbolcount statecount =
    Map.Map (Detect symbolcount statecount) (Update symbolcount statecount)

type Tape symbolcount =
    Map.Map Integer (Finite symbolcount)

operate ::
    Machine symbolcount statecount -> Finite symbolcount -> Status symbolcount statecount -> Status symbolcount statecount

operate machine defSym (state, position, tape) =
    let symbol = Map.findWithDefault defSym position tape
        defUpd = (defSym, state, N)
        (symbol', state', motion) =
            Map.findWithDefault defUpd (symbol, state) machine
        position' = move motion position
        tape' = Map.insert position symbol' tape
    in  (state', position', tape')