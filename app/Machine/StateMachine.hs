module Machine.StateMachine where

import Data.List (find)
type Action = String
type Name = String
type ID = Int
type CharacterName = Name
type ComboName = Name

data Transition = Transition { actions :: [Action]
                             , nextStateId :: ID
                             } deriving (Show, Read, Eq)

type State = (ID, [Transition], [(CharacterName, ComboName)])

type DeltaFunc = AlityMachine -> ID -> [Action] -> (Bool, Maybe ID)

data AlityMachine = AlityMachine { alaphabet :: [[Action]]
                                 , states :: [State]
                                 , initialStateId :: ID
                                 , finalStatesId :: [ID]
                                 , delta :: DeltaFunc
                                 }

-- deltaFunction iterate over a list of transition and return the nextStateId of the transition with the corresponding actions
deltaFunction :: DeltaFunc
deltaFunction machine currentStateId inputActions =
    let initStateId = initialStateId machine
    in case getStateById machine currentStateId >>= \(_, t, _) -> getTransitionByAction t inputActions of
        Just t -> (initStateId == currentStateId, Just (nextStateId t))
        Nothing ->
            case getStateById machine initStateId
                 >>= \(_, t, _) -> getTransitionByAction t inputActions
                 >>= \newt -> Just (nextStateId newt)
            of
            Just newId -> (True, Just newId)
            Nothing -> (True, Just initStateId)
    where
      getStateById :: AlityMachine -> ID -> Maybe State
      getStateById curMachine targetId = find (\ (stateID, _, _) -> stateID == targetId) (states curMachine)
      getTransitionByAction :: [Transition] -> [Action] -> Maybe Transition
      getTransitionByAction tlst actlst = find (\ t -> actions t == actlst) tlst
