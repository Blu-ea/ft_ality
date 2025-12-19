module Utils where
import Data.Foldable (Foldable(toList))

import SDL
import Data.Char (toLower)


printList :: (Show a) => [a] -> IO()
printList (x:xs) = do
    print x
    printList xs
printList [] = return ()

--  This works also with Monads such as `Just` or `Node`
printFoldable :: (Foldable t, Show a) => t a -> IO() 
printFoldable = printList . toList


printEventType :: EventPayload -> IO()
printEventType (KeyboardEvent e) = print $ "This is a keyboard Event " ++ (show . keyboardEventKeyMotion $ e) ++ " " ++ (show . keysymKeycode . keyboardEventKeysym $ e)
printEventType _ = print "Unknowed event\n"

-- | Will return the corresponding KeyCode depending of the input String.  
-- If nothing is found, it will return `KeycodeUnknown`
nameToKeycode :: String -> Keycode
nameToKeycode [c] = charToKeycode $ toLower c
nameToKeycode "left" = KeycodeLeft
nameToKeycode "right" = KeycodeRight
nameToKeycode "up" = KeycodeUp
nameToKeycode "down" = KeycodeDown
nameToKeycode "space" = KeycodeSpace
nameToKeycode _ = KeycodeUnknown

charToKeycode :: Char -> Keycode
charToKeycode c = Keycode (fromIntegral . fromEnum $ c)

isKeyPress :: KeyboardEventData -> Bool
isKeyPress ke = keyboardEventKeyMotion ke == Pressed && not (keyboardEventRepeat ke)
isKeyRelease :: KeyboardEventData -> Bool
isKeyRelease ke = keyboardEventKeyMotion ke == Released
