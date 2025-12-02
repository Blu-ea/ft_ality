module Parsing.AlityLexer (Token(..), lexer) where

import Data.Char
import Data.List

data Token = SemiColon
           | Colon
           | Comma
           | Equal
           | Greater
           | Pipe
           | Plus
           | LBrace
           | RBrace
           | Text String
           | Combos
           | Keys
           | Unknown Char
           deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c == ';'  = SemiColon : lexer cs
    | c == ':'  = Colon     : lexer cs
    | c == ','  = Comma     : lexer cs
    | c == '='  = Equal     : lexer cs
    | c == '>'  = Greater   : lexer cs
    | c == '|'  = Pipe      : lexer cs
    | c == '+'  = Plus      : lexer cs
    | c == '{'  = LBrace    : lexer cs
    | c == '}'  = RBrace    : lexer cs

    | c == '"'  = let (str, strTail) = readText cs
                  in Text str : lexer strTail

    | "keys" `isPrefixOf` (c:cs) = Keys : lexer (drop 4 (c:cs))

    | "combos" `isPrefixOf` (c:cs) = Combos : lexer (drop 6 (c:cs))

    | otherwise = Unknown c : lexer cs

readText :: String -> (String, String)
readText str = let strPrefix = takeWhile (/='"') str
                   strTail   = drop (length strPrefix + 1) str
               in (strPrefix, strTail)

