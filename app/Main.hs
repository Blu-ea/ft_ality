module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)

import qualified SDL
    ( Keycode,
      initialize,
      createWindow,
      defaultWindow,
      destroyWindow,
      InitFlag(InitJoystick, InitEvents, InitGameController) )

import Data.Text (pack) -- Needed to create the window title
import Data.List (intercalate, sort)

import EventListner ( getEventFilter, reduceEventList )

import Parsing.FileMachine as FM
import Machine.StateMachine (AlityMachine(..), State, Action, deltaFunction, getInitState, getStateById)
import qualified Parsing.AlityParser as Parser
import Utils
import System.Directory.Internal.Prelude (when)

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
        Error -> putStrLn "Error on usage\nUsage: ft_ality [combo file]"
        Help -> printUsage
        File file -> runProgram file

runProgram :: String -> IO ()
runProgram filePath = do
    fileExist <- doesFileExist filePath
    if not fileExist
        then putStrLn $ "Error: File " ++ filePath ++ " does not exist."
        else do

            fileStr <- readFile filePath
            case FM.stringToMachine fileStr deltaFunction of
                Parser.Error err -> putStrLn $ "Error parsing inputs file: " ++ err
                Parser.Success (bindings, machine) ->
                    let bindingsCode = map (\(Parser.KeyBinding name action) -> (nameToKeycode name, action)) bindings in do

                        SDL.initialize [SDL.InitEvents, SDL.InitGameController, SDL.InitJoystick]
                        window <- SDL.createWindow  (pack "ft_ality | Key1-detector") SDL.defaultWindow

                        ls <- getEventFilter $ map fst bindingsCode
                        processLoop (reduceEventList ls) [](bindingsCode, machine, getInitState machine)

                        SDL.destroyWindow window

processLoop :: [[SDL.Keycode]] -> [[String]] -> ([(SDL.Keycode, Action)], AlityMachine, State) -> IO ()
processLoop [] _ _ = putStrLn "Quitting..."
processLoop (x:xs) actHistory (bindings, machine, (currentId, _, _)) = do
    let actList = sort $ keycodesToActions x bindings
        (backToInit, newStateId) = delta machine machine currentId actList in
        case newStateId of
            Nothing -> putStrLn $ "No transition found with actions " ++ show actList
            Just newId ->
                case getStateById machine newId of
                    Nothing -> putStrLn $ "No transition found with actions " ++ show actList
                    Just ns@(_, _, nextCombo) -> do
                        let newHistory = if backToInit then [actList] else actHistory ++ [actList]
                        putChar '\n'
                        when backToInit $ putStrLn " == == == == "
                        printActions newHistory
                        mapM_ (\(charName, combo) -> putStrLn $ combo ++ " (" ++ charName ++ ")" ++ " !!") nextCombo
                        processLoop xs newHistory (bindings, machine, ns)

keycodesToActions :: [SDL.Keycode] -> [(SDL.Keycode, Action)] -> [Action]
keycodesToActions keycodes bindings =
    [action | (kc, action) <- bindings, kc `elem` keycodes]

printActions :: [[Action]] -> IO ()
printActions [] = return ()
printActions actLists = do
    let actStrs = map (intercalate ", ") actLists
    putStrLn $ intercalate " -> " actStrs
