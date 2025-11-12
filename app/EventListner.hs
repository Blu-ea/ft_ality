module EventListner where

import SDL
import Control.Monad
import Utils (printList)


appLoop :: Renderer -> Bool -> IO ()
appLoop renderer isAPressed = do
    events <- pollEvents

    unless (null events) . print $ length events
    printList events
    let eventIsQPress event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
        qPressed =  any eventIsQPress events
    if isAPressed
        then rendererDrawColor renderer $= V4 0 0 255 255
        else rendererDrawColor renderer $= V4 0 255 0 255

    let aPressed = any (eventKeyIsHold  KeycodeA) events
    clear renderer
    present renderer
    when aPressed $ print events
    if aPressed
        then unless qPressed $ appLoop renderer $ not isAPressed
        else unless qPressed $ appLoop renderer isAPressed





watchKeyboardInput:: Event -> IO()
watchKeyboardInput event = do

    case eventPayload event of 
        WindowClosedEvent closeEvent -> print "Window is closed :D\n"
        KeyboardEvent keyboardEvent -> print "Key is event :D\n"
        _ -> return ()

    -- let eventIsQPress event =
    --         case eventPayload event of
    --             KeyboardEvent keyboardEvent ->
    --                 keyboardEventKeyMotion keyboardEvent == Pressed &&
    --                 keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    --             _ -> False
    --     qPressed =  any eventIsQPress events
    -- if isAPressed
    --     then rendererDrawColor renderer $= V4 0 0 255 255
    --     else rendererDrawColor renderer $= V4 0 255 0 255

    -- let aPressed = any (eventKeyIsHold  KeycodeA) events
    -- clear renderer
    -- present renderer
    -- when aPressed $ print events
    -- if aPressed
    --     then unless qPressed $ appLoop renderer $ not isAPressed
    --     else unless qPressed $ appLoop renderer isAPressed

eventKeyIsHold :: Keycode -> Event -> Bool
eventKeyIsHold keycode event =
            case eventPayload event of

                KeyboardEvent keyboardEvent ->
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode &&
                    keyboardEventKeyMotion keyboardEvent == Pressed && not (keyboardEventRepeat keyboardEvent)

                _ -> False
