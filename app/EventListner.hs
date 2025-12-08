module EventListner where

import SDL
import GHC.IO (unsafeInterleaveIO)
import Data.Maybe (isNothing, fromJust)
import Utils ( isKeyPress, isKeyRelease )
import Data.List ( delete )

getEventFilter :: [Keycode] -> IO[Event]
getEventFilter inputKeys = unsafeInterleaveIO $ do
    event <- pollEvent
    events <- getEventFilter inputKeys
    
    if isNothing event then return events else 
        let e = fromJust event in 
        case eventPayload e of
            KeyboardEvent ke | isEscapeKey ke
                -> return []
            WindowClosedEvent _ 
                -> return []

            KeyboardEvent ke | isInputKey ke && (isKeyPress ke || isKeyRelease ke)
                -> return (e : events)

            _ -> return events
    where
    isInputKey ke = keysymKeycode(keyboardEventKeysym ke) `elem` inputKeys
    isEscapeKey ke = keysymKeycode (keyboardEventKeysym ke) == KeycodeEscape 


-- If event is a press, We add it to keyPressed and accumulator
    -- then call `reduce es keypressed acc` with keypressed and the accumulator updated
-- If event is a released, We remove it from the keypressed
    -- If the Keypressed is empty and not the acc, we return the `acc : (recude es [] [])`
    -- otherwise we call `reduce es Keypressed acc`

reduceEventList :: [Event] -> [[Keycode]]
reduceEventList events = reduceEventList' events [] []

reduceEventList' :: [Event] -> [Keycode] -> [Keycode] -> [[Keycode]]
reduceEventList' [] _ _ = []
reduceEventList' (e:es) keyPressed acc = case eventPayload e of 

    KeyboardEvent ke | isKeyPress ke
        -> let keycodePressed = keysymKeycode (keyboardEventKeysym ke) :: Keycode in
            reduceEventList' es (keycodePressed : keyPressed) (if keycodePressed `notElem` acc then keycodePressed : acc else acc)

    KeyboardEvent ke | isKeyRelease ke
        -> let keycodeReleased = keysymKeycode (keyboardEventKeysym ke) :: Keycode in
            let res = reduceEventList' es (delete keycodeReleased keyPressed) in
            if keyPressed == [keycodeReleased]
                then acc:res []
                else res acc

    _ -> reduceEventList' es keyPressed acc


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