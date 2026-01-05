module EventListner where

import qualified SDL
import System.IO.Unsafe (unsafeInterleaveIO)
import Utils ( isKeyPress, isKeyRelease )
import Data.List ( delete )

getEventFilter :: [SDL.Keycode] -> IO[SDL.Event]
getEventFilter inputKeys = unsafeInterleaveIO $ do
    event <- SDL.waitEvent
    events <- getEventFilter inputKeys
    
    case SDL.eventPayload event of
        SDL.KeyboardEvent ke | isEscapeKey ke
            -> return []
        SDL.WindowClosedEvent _ 
            -> return []

        SDL.KeyboardEvent ke | isInputKey ke && (isKeyPress ke || isKeyRelease ke)
            -> return (event : events)

        _ -> return events
    where
    isInputKey ke = SDL.keysymKeycode(SDL.keyboardEventKeysym ke) `elem` inputKeys
    isEscapeKey ke = SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeEscape 


-- If event is a press, We add it to keyPressed and accumulator
    -- then call `reduce es keypressed acc` with keypressed and the accumulator updated
-- If event is a released, We remove it from the keypressed
    -- If the Keypressed is empty and not the acc, we return the `acc : (recude es [] [])`
    -- otherwise we call `reduce es Keypressed acc`

reduceEventList :: [SDL.Event] -> [[SDL.Keycode]]
reduceEventList events = reduceEventList' events [] []

reduceEventList' :: [SDL.Event] -> [SDL.Keycode] -> [SDL.Keycode] -> [[SDL.Keycode]]
reduceEventList' [] _ _ = []
reduceEventList' (e:es) keyPressed acc = case SDL.eventPayload e of 

    SDL.KeyboardEvent ke | isKeyPress ke
        -> let keycodePressed = SDL.keysymKeycode (SDL.keyboardEventKeysym ke) :: SDL.Keycode in
            reduceEventList' es (keycodePressed : keyPressed) (if keycodePressed `notElem` acc then keycodePressed : acc else acc)

    SDL.KeyboardEvent ke | isKeyRelease ke
        -> let keycodeReleased = SDL.keysymKeycode (SDL.keyboardEventKeysym ke) :: SDL.Keycode in
            let res = reduceEventList' es (delete keycodeReleased keyPressed) in
            if keyPressed == [keycodeReleased]
                then acc:res []
                else res acc

    _ -> reduceEventList' es keyPressed acc
