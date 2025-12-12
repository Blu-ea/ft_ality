module Main where

import SDL
    ( Keycode,
      initialize,
      createWindow,
      defaultWindow,
      destroyWindow,
      InitFlag(InitJoystick, InitEvents, InitGameController) )
import Data.Text(pack)

import SDL.Input.GameController ( availableControllers )
import EventListner ( getEventFilter, reduceEventList )
import SDL.Internal.Numbered (FromNumber(fromNumber), ToNumber(toNumber))
import System.Environment
import System.Directory (doesFileExist)

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
            initialize [InitEvents, InitGameController, InitJoystick]

            window <- createWindow  (pack "ft_ality | Key1-detector") defaultWindow
            -- renderer <- createRenderer window (-1) defaultRenderer

            test <- availableControllers
            print test

            ls <- getEventFilter $ map fromNumber [97 .. 122] -- Get all the letters
            process $ reduceEventList ls

            destroyWindow window

process :: [[Keycode]] -> IO ()
process (x:xs) = do
    process' x
    process xs
process [] = print "Ending the process !"


process' :: [Keycode] -> IO()
process' (x:xs) = do
    print $ toNumber x
    process' xs
process' [] = print "--- new input ---"
