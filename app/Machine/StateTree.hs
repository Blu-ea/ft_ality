module Machine.StateTree where

import Parsing.AlityParser
import Machine.StateMachine
import Data.List (find, nub, delete)

data StateTree = Empty | StateTreeNode { nodeActions :: [Action]
                                       , nodeCombo :: [ComboAssociation]
                                       , nextTrees :: [StateTree]
                                       } deriving (Show, Eq)

sameAct :: StateTree -> StateTree -> Bool
sameAct t1 t2 = nodeActions t1 == nodeActions t2

removeSameAction :: StateTree -> [StateTree] -> [StateTree]
removeSameAction Empty lst = lst
removeSameAction t@(StateTreeNode {}) [] = [t]
removeSameAction t1@(StateTreeNode {}) lst = filter (not . sameAct t1) lst

singletonStateTree :: [Action] -> [ComboAssociation] -> StateTree
singletonStateTree act asso = StateTreeNode { nodeActions = act
                                            , nodeCombo = asso
                                            , nextTrees = []
                                            }

-- List insert method

findByAction :: [Action] -> [StateTree] -> Maybe StateTree
findByAction act = find (\t -> nodeActions t == act)

insertInTree :: [[Action]] -> [ComboAssociation] -> StateTree -> StateTree
insertInTree [] newAsso (StateTreeNode acts asso next) = StateTreeNode acts (newAsso ++ asso) next
insertInTree acts newAsso Empty = singleBranchStateTree acts newAsso
insertInTree (act:rest) newAsso (StateTreeNode tact asso next) =
    case findByAction act next of
        Nothing -> StateTreeNode tact asso (singleBranchStateTree (act:rest) newAsso : next)
        Just t -> StateTreeNode tact asso (insertInTree rest newAsso t : delete t next)

-- Tree and mergin aproach below

singleBranchStateTree :: [[Action]] -> [ComboAssociation] -> StateTree
singleBranchStateTree [] _ = Empty
singleBranchStateTree [actlist] asso = singletonStateTree actlist asso
singleBranchStateTree (firstAct: restAct) asso =
    StateTreeNode { nodeActions = firstAct
                  , nodeCombo = []
                  , nextTrees = [singleBranchStateTree restAct asso]
                  }

insertStateTree :: StateTree -> StateTree -> StateTree
insertStateTree Empty t2 = t2
insertStateTree t1 Empty = t1
insertStateTree t1 (StateTreeNode acts2 asso2 nexts2) =
    let (dups, rest) = partition (sameAct t1) nexts2
    in if null dups
       then StateTreeNode acts2 asso2 (t1 : nexts2)
       else StateTreeNode acts2 asso2 (foldl mergeStateTreeIn t1 dups : rest)
    where
      partition _ [] = ([], [])
      partition p (x:xs)
        | p x       = let (ts, fs) = partition p xs in (x:ts, fs)
        | otherwise = let (ts, fs) = partition p xs in (ts, x:fs)

mergeStateTreeIn :: StateTree -> StateTree -> StateTree
mergeStateTreeIn Empty t2 = t2
mergeStateTreeIn t1 Empty = t1
mergeStateTreeIn (StateTreeNode _ asso1 nexts1) (StateTreeNode acts2 asso2 nexts2) =
    StateTreeNode acts2 (nub (asso1 ++ asso2)) (mergeStateTreeList nexts1 nexts2)

mergeStateTreeList :: [StateTree] -> [StateTree] -> [StateTree]
mergeStateTreeList [] lst = lst
mergeStateTreeList lst [] = lst
mergeStateTreeList (current:restStates) states2 =
    case find (sameAct current) states2 of
        Nothing -> current : mergeStateTreeList restStates states2
        Just t -> mergeStateTreeIn current t : mergeStateTreeList restStates (delete t states2)

-- Create root (q0) of the state tree, then insert each branch in the root to create the final tree
