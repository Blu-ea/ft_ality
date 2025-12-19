module Parsing.FileMachine where

import Parsing.AlityLexer as Lex
import Parsing.AlityParser as Parse
import Machine.StateTree as STree
import Machine.StateMachine as Machine

stringToMachine :: String -> Machine.DeltaFunc -> ParseResult ([Parse.KeyBinding], Machine.AlityMachine)
stringToMachine s f =
    case parseStr s of
        Error err -> Error err
        Success (bindings, tree) ->
            let machine = convertTreeToMachine tree f
            in Success (bindings, machine)

parseStr :: String -> ParseResult ([Parse.KeyBinding], STree.StateTree)
parseStr s =
    let tokens = Lex.lexer s 1 1
    in case Parse.parseKeysSection tokens of
        Error err -> Error err
        Success (keyBindings, restTokens) ->
            case Parse.parseCombosSection restTokens of
                Error err -> Error err
                Success (comboRules, _) ->
                    let tree = createTreeFromRules comboRules
                    in Success (keyBindings, tree)
            where
                createTreeFromRules [] = STree.singletonStateTree [] []
                createTreeFromRules lst = foldl (\tree (ComboRule actList assoList) -> STree.insertInTree actList assoList tree) 
                                                (STree.singletonStateTree [] [])
                                                lst

convertTreeToMachine :: STree.StateTree -> Machine.DeltaFunc -> Machine.AlityMachine
convertTreeToMachine tree f =
    let (statesList, alphaList, finalIdsList, _) = treeToMachine tree 0 [] [] []
    in Machine.AlityMachine { alaphabet = alphaList
                            , states = statesList
                            , initialStateId = 0
                            , finalStatesId = finalIdsList
                            , delta = f
                            }

treeToMachine :: STree.StateTree -> ID -> [Machine.State] -> [[Machine.Action]] -> [ID] -> ([Machine.State], [[Machine.Action]], [ID], ID)
treeToMachine Empty currentId statesList actList finalIdsList = (statesList, actList, finalIdsList, currentId)
treeToMachine (StateTreeNode nodeActs nodeComboAssoc subtrees) currentId statesList actList finalIdsList =
    let
        acts1 = if null nodeActs then actList else addToAlphabet nodeActs actList
        isFinal = not (null nodeComboAssoc)
        finalIds1 = if isFinal then currentId : finalIdsList else finalIdsList
        comboTuples = map (\ca -> (characterName ca, comboName ca)) nodeComboAssoc
        (stateLst, acts2, finalIds2, nextId, trans) =
            processSubtrees subtrees (currentId + 1) statesList acts1 finalIds1 []
        currentState = (currentId, trans, comboTuples)
    in (currentState : stateLst, acts2, finalIds2, nextId)
    where
    processSubtrees :: [StateTree] -> ID -> [State] -> [[Action]] -> [ID] -> [Transition] ->
                       ([State], [[Action]], [ID], ID, [Transition])
    processSubtrees [] nextId accStates accAlph accFinalIds accTrans =
        (accStates, accAlph, accFinalIds, nextId, accTrans)

    processSubtrees (tree:rest) nextId accStates accAlph accFinalIds accTrans =
        let (subStates, subAlph, subFinalIds, newNextId) =
                treeToMachine tree nextId accStates accAlph accFinalIds
            subTreeActs = case tree of
                Empty -> []
                (StateTreeNode acts _ _) -> acts
            newTrans = Transition { Machine.actions = subTreeActs, nextStateId = nextId }
            (finalStates, finalAlph, finalFinalIds, finalNextId, allTrans) =
                processSubtrees rest newNextId subStates subAlph subFinalIds (newTrans : accTrans)
        in (finalStates, finalAlph, finalFinalIds, finalNextId, allTrans)

addToAlphabet :: [Action] -> [[Action]] -> [[Action]]
addToAlphabet act alphaList =
    if act `elem` alphaList then alphaList else act : alphaList
