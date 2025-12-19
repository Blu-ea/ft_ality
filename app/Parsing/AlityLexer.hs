module Parsing.AlityLexer (Token(..), TokenType(..), lexer) where

import Data.Char ( isSpace )
import Data.List ( isPrefixOf )

data TokenType = SemiColon
               | Colon
               | Comma
               | Equal
               | Greater
               | Pipe
               | Plus
               | LBrace
               | RBrace
               | Text
               | Combos
               | Keys
               | Unknown
               deriving (Show, Eq)

data Token = Token { tokenType :: TokenType
                   , str       :: String
                   , lineNum   :: Int
                   , colNum    :: Int
                   } deriving (Show, Eq)

lexer :: String -> Int -> Int -> [Token]
lexer [] _ _ = []
lexer (c:cs) line col
    | isSpace c = lexer cs (line + if c == '\n' then 1 else 0) (if c == '\n' then 1 else col + 1)
    | c == ';'  = Token SemiColon [c] line col : lexer cs line (col + 1)
    | c == ':'  = Token Colon [c] line col     : lexer cs line (col + 1)
    | c == ','  = Token Comma [c] line col     : lexer cs line (col + 1)
    | c == '='  = Token Equal [c] line col     : lexer cs line (col + 1)
    | c == '>'  = Token Greater [c] line col   : lexer cs line (col + 1)
    | c == '|'  = Token Pipe [c] line col      : lexer cs line (col + 1)
    | c == '+'  = Token Plus [c] line col      : lexer cs line (col + 1)
    | c == '{'  = Token LBrace [c] line col    : lexer cs line (col + 1)
    | c == '}'  = Token RBrace [c] line col    : lexer cs line (col + 1)

    | c == '"'  = let (s, strTail) = readText cs
                  in Token Text s line col : lexer strTail line (col + length s + 2)

    | "keys" `isPrefixOf` (c:cs) = Token Keys "keys" line col : lexer (drop 4 (c:cs)) line (col + 4)

    | "combos" `isPrefixOf` (c:cs) = Token Combos "combos" line col : lexer (drop 6 (c:cs)) line (col + 6)

lexer (c:cs) line col = 
    let unknownStr = takeWhile (\x -> not (isSpace x) && notElem x [';', ':', ',', '=', '>', '|', '+', '{', '}', '"']) (c:cs)
        restStr = drop (length unknownStr) (c:cs)
    in
    Token Unknown unknownStr line col : lexer restStr line (col + 1)

readText :: String -> (String, String)
readText s = let strPrefix = takeWhile (/='"') s
                 strTail   = drop (length strPrefix + 1) s
               in (strPrefix, strTail)
