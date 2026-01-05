module Utils where
import Data.Foldable (Foldable(toList))

import qualified SDL
import Data.Char (toLower)


printList :: (Show a) => [a] -> IO()
printList (x:xs) = do
    print x
    printList xs
printList [] = return ()

--  This works also with Monads such as `Just` or `Node`
printFoldable :: (Foldable t, Show a) => t a -> IO() 
printFoldable = printList . toList


printEventType :: SDL.EventPayload -> IO()
printEventType (SDL.KeyboardEvent e) = print $ "This is a keyboard Event " ++ (show . SDL.keyboardEventKeyMotion $ e) ++ " " ++ (show . SDL.keysymKeycode . SDL.keyboardEventKeysym $ e)
printEventType _ = print "Unknowed event\n"

-- | Will return the corresponding KeyCode depending of the input String.  
-- If nothing is found, it will return `KeycodeUnknown`
nameToKeycode :: String -> SDL.Keycode
nameToKeycode [c] = charToKeycode $ toLower c
nameToKeycode "left" = SDL.KeycodeLeft
nameToKeycode "right" = SDL.KeycodeRight
nameToKeycode "up" = SDL.KeycodeUp
nameToKeycode "down" = SDL.KeycodeDown
nameToKeycode "space" = SDL.KeycodeSpace
nameToKeycode _ = SDL.KeycodeUnknown

charToKeycode :: Char -> SDL.Keycode
charToKeycode c = SDL.Keycode (fromIntegral . fromEnum $ c)

isKeyPress :: SDL.KeyboardEventData -> Bool
isKeyPress ke = SDL.keyboardEventKeyMotion ke == SDL.Pressed && not (SDL.keyboardEventRepeat ke)
isKeyRelease :: SDL.KeyboardEventData -> Bool
isKeyRelease ke = SDL.keyboardEventKeyMotion ke == SDL.Released
