module Main where

import System.Environment
import System.Directory (doesFileExist)

import SDL
    ( Keycode,
      initialize,
      createWindow,
      defaultWindow,
      destroyWindow,
      InitFlag(InitJoystick, InitEvents, InitGameController) )
import SDL.Input.GameController ( availableControllers )
import SDL.Internal.Numbered (FromNumber(..))

import Data.Text(pack)
import Data.Foldable (find)
import Data.List (intercalate, sort)

import EventListner ( getEventFilter, reduceEventList )

import Parsing.FileMachine as FM
import Machine.StateMachine (AlityMachine(..), State, Action, deltaFunction, initialStateId)
import Parsing.AlityParser as Parser
import Utils

data ArgType = Error | Help | File String deriving (Show, Eq)

checkArgs :: [String] -> ArgType
checkArgs [] = Error
checkArgs ["--help"] = Help
checkArgs ["-h"] = Help
checkArgs [file] = File file
checkArgs _ = Error

printUsage :: IO ()
printUsage = putStrLn "Usage: ft_ality [combo file]"

main :: IO ()
main = do
    args <- getArgs
    case checkArgs args of
        Error -> printUsage
        Help -> printUsage
        File file -> runProgram file

runProgram :: String -> IO ()
runProgram filePath = do
    fileExist <- doesFileExist filePath
    if not fileExist
        then putStrLn $ "Error: File " ++ filePath ++ " does not exist."
        else do

            fileStr <- readFile filePath
            let (bindings, machine) = FM.stringToMachine fileStr deltaFunction -- TODO error handling
                bindingsCode = map (\(Parser.KeyBinding name action) -> (nameToKeycode name, action)) bindings
                initialMachineState = find (\ (stateID, _, _) -> stateID == initialStateId machine) (states machine)

            case initialMachineState of
                Nothing -> putStrLn "Error: Initial state not found in machine states."
                Just initialState -> do

                    initialize [InitEvents, InitGameController, InitJoystick]
                    window <- createWindow  (pack "ft_ality | Key1-detector") defaultWindow
                    -- renderer <- createRenderer window (-1) defaultRenderer

                    test <- availableControllers
                    print test

                    ls <- getEventFilter $ map fromNumber [97 .. 122] -- Get all the letters
                    processLoop (reduceEventList ls) (bindingsCode, machine, initialState)

                    destroyWindow window

processLoop :: [[Keycode]] -> ([(Keycode, Action)], AlityMachine, State) -> IO ()
processLoop [] _ = print "Quitting..."
processLoop (x:xs) (bindings, machine, (currentId, _, _)) = do
    let actList = sort $ keycodesToActions x bindings
    putChar '\n'
    printActions [actList]
    let newStateId = delta machine machine currentId actList
        in case newStateId of
            Nothing -> putStrLn $ "No transition found from state id " ++ show currentId ++ " with actions " ++ show actList
            Just newId ->
                let nextState = find (\ (stateID, _, _) -> stateID == newId) (states machine)
                in case nextState of
                    Nothing -> putStrLn $ "No transition found from state id " ++ show currentId ++ " with actions " ++ show actList
                    Just ns@(_, _, nextCombo) -> do
                        mapM_ (\(charName, combo) -> putStrLn $ combo ++ " (" ++ charName ++ ")" ++ " !!") nextCombo
                        processLoop xs (bindings, machine, ns)

keycodesToActions :: [Keycode] -> [(Keycode, Action)] -> [String]
keycodesToActions keycodes bindings =
    [ action | kc <- keycodes, (bindKc, action) <- bindings, kc == bindKc ]

printActions :: [[Action]] -> IO ()
printActions [] = return ()
printActions actLists = do
    let actStrs = map (intercalate ", ") actLists
    putStrLn $ intercalate " -> " actStrs
