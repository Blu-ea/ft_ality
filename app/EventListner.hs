module EventListner where

import SDL
import Control.Monad
import GHC.IO (unsafeInterleaveIO)
import Data.Maybe (isNothing, fromJust)


appLoop :: Renderer -> Bool -> GameState -> IO ()
appLoop renderer isAPressed gs = do
    events <- pollEvents

    unless (null events) . print $ length events
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
        then unless qPressed $ appLoop renderer (not isAPressed) gs
        else unless qPressed $ appLoop renderer isAPressed gs


newtype GameState = GameState {
    pressed :: [Keycode]
}
defaultGameState :: GameState
defaultGameState = GameState {pressed = []}

allKeyUp :: GameState -> Bool
allKeyUp = null . pressed


watchKeyboardInput:: Event -> IO()
watchKeyboardInput event = do

    case eventPayload event of
        WindowClosedEvent _ -> print "Window is closed :D\n"
        KeyboardEvent _ -> print "Key is event :D\n"
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

test' :: IO[Event]
test' = unsafeInterleaveIO $ do
    event <- waitEvent
    events <- test'
    case eventPayload event of

        KeyboardEvent e |
            keyboardEventKeyMotion e == Pressed && not (keyboardEventRepeat e)
                -> return (event : events)

        KeyboardEvent e |
            keyboardEventKeyMotion e == Released
                -> return (event : events)

        _ -> return events


getEventFilter :: [Keycode] -> IO[Event]
getEventFilter inputKeys = unsafeInterleaveIO $ do
    event <- pollEvent
    events <- getEventFilter inputKeys

    if isNothing event then return events else 
        let e = fromJust event in 
        case eventPayload e of
            KeyboardEvent ke | keysymKeycode (keyboardEventKeysym ke) == KeycodeEscape 
                -> return []
            WindowClosedEvent _ 
                -> return []

            KeyboardEvent ke | t ke
                -> pure (e : events)
            _ -> pure events
    where t ke = keysymKeycode(keyboardEventKeysym ke) `elem` inputKeys



    -- -- When Ctl+c is pressed, require an other input to leave the processe
    -- event <- waitEvent
    -- events <- getEventFilter
    -- case eventPayload event of
    --     paylaod@(KeyboardEvent _)| t paylaod
    --             -> pure (event : events)
    --     _ -> pure events
    --   --If end -> return []
    -- where t _ = True


    -- -- Doesn't work corectlly because i want to leave if i found an exit event, but there, it will still continue to loop
    -- event <- pollEvents
    -- events <- getEventFilter
    -- return $ filter t event ++ events
    -- where t _ = True