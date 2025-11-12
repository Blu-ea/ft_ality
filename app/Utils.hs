module Utils where
import Data.Foldable (Foldable(toList))



printList :: (Show a) => [a] -> IO()
printList (x:xs) = do
    print x
    printList xs
printList [] = return ()

--  This works also with Monads such as `Just` or `Node`
printFoldable :: (Foldable t, Show a) => t a -> IO() 
printFoldable = printList . toList
