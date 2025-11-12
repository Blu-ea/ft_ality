module Main where

import SDL
import SDL.Event
import Data.Text(pack)
import SDL.Video
import Control.Monad (unless, when)
import SDL.Input.GameController
import GHC.Base (VecCount(Vec2))
import EventListner


main :: IO ()
main = do
    initialize [InitEvents, InitGameController, InitJoystick]

    window <- createWindow  (pack "ft_ality | Key-detector") defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer

    test <- availableControllers 
    print test

    -- _ <- addEventWatch watchKeyboardInput  -- Cannot probably use addEventWatcher because of the requirement of keeping the current states

    appLoop renderer False

    destroyWindow window
