module Parsing.FileMachine where

import Parsing.AlityLexer as Lex
import Parsing.AlityParser as Parse
import Machine.StateTree as STree
import Machine.StateMachine as Machine

stringToMachine :: String -> Machine.DeltaFunc -> ([Parse.KeyBinding], Machine.AlityMachine)
stringToMachine str f =
    let (bindings, tree) = parseStr str
        machine = convertTreeToMachine tree f
    in (bindings, machine)

parseStr :: String -> ([Parse.KeyBinding], STree.StateTree)
parseStr str =
    let tokens = Lex.lexer str
        (keyBindings, restTokens) = Parse.parseKeysSection tokens
        (comboRules, _) = Parse.parseCombosSection restTokens
        tree = createTreeFromRules comboRules
    in (keyBindings, tree)
    where
        createTreeFromRules [] = STree.singletonStateTree [] []
        createTreeFromRules lst = foldl (\tree (ComboRule actList assoList) -> STree.insertInTree actList assoList tree) (STree.singletonStateTree [] []) lst

convertTreeToMachine :: STree.StateTree -> (AlityMachine -> ID -> [Action] -> Maybe ID) -> AlityMachine
convertTreeToMachine tree f =
    let (statesList, alphaList, finalIdsList, _) = treeToMachine tree 0 [] [] []
    in AlityMachine { alaphabet = alphaList
                    , states = statesList
                    , initialStateId = 0
                    , finalStatesId = finalIdsList
                    , delta = f
                    }

treeToMachine :: StateTree -> ID -> [State] -> [[Action]] -> [ID] -> ([State], [[Action]], [ID], ID)
treeToMachine Empty currentId statesList alphaList finalIdsList = (statesList, alphaList, finalIdsList, currentId)
treeToMachine (StateTreeNode nodeActs nodeComboAssoc subtrees) currentId statesList alphaList finalIdsList =
    let
        alpha1 = if null nodeActs then alphaList else addToAlphabet nodeActs alphaList
        isFinal = not (null nodeComboAssoc)
        finalIds1 = if isFinal then currentId : finalIdsList else finalIdsList
        comboTuples = map (\ca -> (characterName ca, comboName ca)) nodeComboAssoc
        (states1, alpha2, finalIds2, nextId, trans) =
            processSubtrees subtrees (currentId + 1) statesList alpha1 finalIds1 []
        currentState = (currentId, trans, comboTuples)
    in (currentState : states1, alpha2, finalIds2, nextId)
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
