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


main :: IO ()
main = do
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
