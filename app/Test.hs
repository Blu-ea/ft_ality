module Test where

-- This file has Only temporary code, for testing purposes 

import Data.List
import Data.Foldable

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunc [] . words  -- We don't take all the input, so folding Func take a List (acc) and a string as input (each args of input)
    where
        foldingFunc (x:y:ys) "+" = (x + y) : ys
        foldingFunc (x:y:ys) "-" = (x - y) : ys
        foldingFunc (x:y:ys) "*" = (x * y) : ys
        foldingFunc (x:y:ys) "/" = (x / y) : ys
        foldingFunc (x:y:ys) "^" = (x ** y) : ys
        foldingFunc (x:xs) "ln" = log x : xs 
        foldingFunc xs "sum" = [sum xs]
        foldingFunc stack numberString = read numberString : stack

-- ["1", "2", "3", "4", "5", "+", "+", "+"]
solveRPN' :: String -> Maybe Float
solveRPN' [] = Nothing
solveRPN' input = 
    let res = foldlM foldingFunc [] $ words input in
    case res of 
        Just xs -> Just $ head xs
        Nothing -> Nothing
    where
        foldingFunc (x:y:ys) "+" = Just $ (x + y) : ys
        foldingFunc (x:y:ys) "-" = Just $ (x - y) : ys
        foldingFunc (x:y:ys) "*" = Just $ (x * y) : ys
        foldingFunc (x:y:ys) "/" = Just $ (x / y) : ys
        foldingFunc (x:y:ys) "^" = Just $ (x ** y) : ys
        foldingFunc (x:xs) "ln" = Just $ log x : xs
        foldingFunc xs "sum" = Just $ [sum xs]
        foldingFunc stack numberString = case reads numberString of 
                [(number, "")] -> Just $ number : stack
                _ -> Nothing

