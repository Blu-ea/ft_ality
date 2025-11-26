module Main where

import SDL
import Data.Text(pack)

import SDL.Input.GameController
import EventListner
import Utils


main :: IO ()
main = do
    initialize [InitEvents, InitGameController, InitJoystick]

    window <- createWindow  (pack "ft_ality | Key-detector") defaultWindow
    -- renderer <- createRenderer window (-1) defaultRenderer

    test <- availableControllers
    print test

    -- _ <- addEventWatch watchKeyboardInput  -- Cannot probably use addEventWatcher because of the requirement of keeping the current states

    -- appLoop renderer False defaultGameState
    ls <- getEventFilter

    process ls

    destroyWindow window

process :: [Event] -> IO ()
process (x:xs) = do
    printEventType $ eventPayload x
    process xs
process [] = print "Ending the process !"
