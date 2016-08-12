module FSM where

import Nats
import Tip
import Test.LazySmallCheck


type Name = Nat
type Label = Nat

eqLabel :: Label -> Label -> Bool
eqLabel = eqNat

data OptLabel = NoLabel | WithLabel Label
  deriving (Show, Eq)

instance Serial OptLabel where
  series = cons0 NoLabel \/ cons1 WithLabel

idleLabel :: Label
idleLabel = Z

data FSMModel = FSMModel { machines :: [FSM] }
  deriving (Show, Eq)

instance Serial FSMModel where
  series = cons1 FSMModel

data FSM = FSM { fsm_name :: Name, states :: [State], initial_state :: Name }
  deriving (Show, Eq)

instance Serial FSM where
  series = cons3 FSM

data State = State { state_name :: Name, leavingTransitions :: [Transition], machine :: Name }
  deriving (Show, Eq)

instance Serial State where
  series = cons3 State

data Transition = Transition { source :: Name, target :: Name, input :: Label, output :: OptLabel }
  deriving (Show, Eq)

instance Serial Transition where
  series = cons4 Transition

addIdleLoop :: FSMModel -> FSMModel
addIdleLoop m = FSMModel (addIdleLoopFSMs (machines m))

addIdleLoopFSMs :: [FSM] -> [FSM]
addIdleLoopFSMs [] = []
addIdleLoopFSMs (fsm:fsms) = addIdleLoopFSM fsm : addIdleLoopFSMs fsms

addIdleLoopFSM :: FSM -> FSM
addIdleLoopFSM fsm = fsm { states = addIdleLoopStates (states fsm) }

addIdleLoopStates :: [State] -> [State]
addIdleLoopStates [] = []
addIdleLoopStates (st:sts) = addIdleLoopState st : addIdleLoopStates sts

addIdleLoopState :: State -> State
addIdleLoopState st | hasIdleLoop (leavingTransitions st) =
                        let stnm = state_name st
                        in st { leavingTransitions = Transition stnm stnm idleLabel NoLabel : leavingTransitions st }
                    | otherwise = st

hasIdleLoop :: [Transition] -> Bool
hasIdleLoop [] = False
hasIdleLoop (t:ts) = (input t `eqNat` idleLabel) || hasIdleLoop ts

prop_idleLoop m = addIdleLoop m === m

lsc_idleLoop = smallCheck 7 (\m -> addIdleLoop m == m)
