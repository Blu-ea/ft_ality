module MachineStates where

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

data AlityMachine = AlityMachine { alaphabet :: [[Action]]
                                 , states :: [State]
                                 , initialStateId :: ID
                                 , finalStatesId :: [ID]
                                 , delta :: State -> [Action] -> ID
                                 }

type Set a = [a]

powerset :: Set a -> Set (Set a)
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

-- deltaFunction iterate over a list of transition and return the nextStateId of the transition with the corresponding actions
deltaFunction :: State -> [Action] -> ID
deltaFunction (_, transitions, _) inputActions =
  maybe 0 nextStateId (find (\ t -> actions t == inputActions) transitions)

exampleMachine :: AlityMachine
exampleMachine = AlityMachine { alaphabet = [ ["punch"]
                                            , ["kick"]
                                            , ["low kick"]
                                            , ["jump"]
                                            , ["punch", "kick"]
                                            , ["jump", "punch"]
                                            , ["lowk kick", "jump"]
                                            ]
                              , states = [ ( 0
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[]
                                           )
                                         , ( 1
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("bob","simple kick" ), ("bart","simple kick"), ("ash","simple kick")]
                                           )
                                         , ( 2
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("bob", "simple punch"), ("bart","simple punch"), ("ash","simple punch")]
                                           )
                                         , ( 3
                                           , [ Transition { actions = ["punch"], nextStateId = 5}
                                             , Transition { actions = ["kick"], nextStateId = 6}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 7}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[]
                                           )
                                         , ( 4
                                           , [ Transition { actions = ["punch"], nextStateId = 8}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 9}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("bob","bob's low kick"), ("bart","bart's low kick")]
                                           )
                                         , ( 5
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("bob", "high punch"), ("bart", "high punch"), ("ash", "high punch")]
                                           )
                                         , ( 6
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("bob", "high kick"), ("bart", "high kick")]
                                           )
                                         , ( 7
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("ash", "flying tornado")]
                                           )
                                         , ( 8
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 11}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 10}
                                             ]
                                           ,[]
                                           )
                                         , ( 9
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 12}
                                             ]
                                           ,[]
                                           )
                                         , ( 10
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("ash", "reverse niagara kick")]
                                           )
                                         , ( 11
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("ash", "niagara kick"), ("bob", "knee to teeth")]
                                           )
                                         , ( 12
                                           , [ Transition { actions = ["punch"], nextStateId = 1}
                                             , Transition { actions = ["kick"], nextStateId = 2}
                                             , Transition { actions = ["jump"], nextStateId = 3}
                                             , Transition { actions = ["low kick"], nextStateId = 4}
                                             , Transition { actions = ["punch", "kick"], nextStateId = 0}
                                             , Transition { actions = ["jump", "punch"], nextStateId = 0}
                                             , Transition { actions = ["low kick", "jump"], nextStateId = 0}
                                             ]
                                           ,[("bart", "What the hell do you want me to do ?")]
                                           )
                                         ]
                              , initialStateId = 0
                              , finalStatesId = [1, 2, 4, 5, 6, 7, 10, 11, 12]
                              , delta = deltaFunction
                              }

stateLoop :: AlityMachine -> State -> [[Action]] -> [String]
stateLoop _ _ [] = []
stateLoop machine currentState (actionsHead:actionsTail) =
      let (currentId, _, combos) = currentState
          newStateId = delta machine currentState actionsHead
          associatedActions = actionsHead
          strTransition = "Current state id = " ++ show currentId
                          ++ " -> Next state id = " ++ show newStateId
                          ++ " | Actions associated to the transition : " ++ show associatedActions
                          ++ " | Found end state in current state for " ++ show combos
      in strTransition : stateLoop machine (states machine !! newStateId) actionsTail

executionLoopTest :: AlityMachine -> [[Action]] -> [String]
executionLoopTest machine actionsLst =
  let currentState = states machine !! initialStateId machine
  in stateLoop machine currentState actionsLst

-- Parsing

type Bindings  = [(String, String)]
type ComboList = [([[Action]], [(Name, String)])]


