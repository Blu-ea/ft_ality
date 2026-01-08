
module Parsing.AlityParser where

import Data.List

import qualified Parsing.AlityLexer as Lex
import Data.Char (toLower)

data ComboAssociation = ComboAssociation { characterName :: String
                                         , comboName :: String
                                         } deriving (Show, Eq, Ord)

data ComboRule = ComboRule { actions :: [[String]]
                           , comboAssociations :: [ComboAssociation]
                           } deriving (Show, Eq)

data InputConfig = InputConfig { keys :: [KeyBinding]
                               , combos :: [ComboRule]
                               } deriving (Show, Eq)

data KeyBinding = KeyBinding { keyName :: String
                             , actionName :: String
                             } deriving (Show, Eq)

type ParseResult a = Either String a

parseErrorMsg :: String -> String -> Int -> Int -> String
parseErrorMsg expected found line col =
    "Expected " ++ expected ++ " instead of \"" ++ found ++ "\" at line " ++ show line ++ ", column " ++ show col

consumeToken :: [Lex.Token] -> Lex.TokenType -> String -> ParseResult (Lex.Token, [Lex.Token])
consumeToken (t:ts) expectedToken expectedStr
    | Lex.tokenType t == expectedToken = return (t, ts)
    | otherwise = Left (parseErrorMsg expectedStr (Lex.str t) (Lex.lineNum t) (Lex.colNum t))
consumeToken [] _ expectedStr = Left ("Expected " ++ expectedStr ++ " but found end of input")

--Keys parsing

parseKeysSection :: [Lex.Token] -> ParseResult ([KeyBinding], [Lex.Token])
parseKeysSection [] = Left "Unexpected end of input while parsing keys section."
parseKeysSection (t : ts) = do 
    (_, afterKeys) <- consumeToken (t:ts) Lex.Keys  "keys keyword"
    (_, afterLBrace) <- consumeToken afterKeys Lex.LBrace "left brace"
    (keyBindings, rest) <- parseKeys afterLBrace
    (_, finalTokens) <- consumeToken rest Lex.RBrace "right brace"
    return (keyBindings, finalTokens)

parseKeys :: [Lex.Token] -> ParseResult ([KeyBinding], [Lex.Token])
parseKeys [] = Left "Unexpected end of input while parsing keys."
parseKeys (token@(Lex.Token t strTok l c) : ts)
    | t == Lex.RBrace = return ([], token : ts)
    | t == Lex.Text = do
        (keyBinding, nextToks) <- parseKeysLine (token : ts)
        (restKeys, finalTokens) <- parseKeys nextToks
        return (keyBinding : restKeys, finalTokens)
    | otherwise = Left (parseErrorMsg "right brace" strTok l c)

parseKeysLine :: [Lex.Token] -> ParseResult (KeyBinding, [Lex.Token])
parseKeysLine tokens = do
    (Lex.Token _ key _ _, keyRestTokens) <- consumeToken tokens Lex.Text "string value"
    (_, eqRestToken) <- consumeToken keyRestTokens Lex.Equal "equal sign"
    (Lex.Token _ action _ _, actRestToken) <- consumeToken eqRestToken Lex.Text "string value"
    (_, finalTokens) <- consumeToken actRestToken Lex.SemiColon "semicolon"
    return (KeyBinding key (map toLower action), finalTokens)

-- Could use monad here with parse key line by partialy construct the return value with function chained for each token type.
-- The succession could return when fail with corresponding error message.

-- Combos Parsing

parseActionsGroupList :: [Lex.Token] -> ParseResult ([[String]], [Lex.Token])
parseActionsGroupList [] = Left "Unexpected end of input while parsing combo actions."
parseActionsGroupList tokens = do
    (actionGroup, nextToks) <- parseSingleActionGroup tokens
    let sortedActionGroup = sort actionGroup in
        case nextToks of
            (Lex.Token Lex.Greater _ _ _ : _) -> return ([sortedActionGroup], nextToks)
            (Lex.Token Lex.Text _ _ _ : _) -> do
                (restActionGroups, finalTokens) <- parseActionsGroupList nextToks 
                return (sortedActionGroup : restActionGroups, finalTokens)
            (Lex.Token _ t l c : _) -> Left (parseErrorMsg "plus or greater" t l c)
            [] -> Left "Unexpected end of input while parsing combo actions."

parseSingleActionGroup :: [Lex.Token] -> ParseResult ([String], [Lex.Token])
parseSingleActionGroup tokens = do
    (action, afterAction) <- parseAction tokens
    case afterAction of
        (Lex.Token Lex.Greater _ _ _ : _) -> return ([action], afterAction)
        (Lex.Token Lex.Comma _ _ _ : restTokens) -> return ([action], restTokens)
        (Lex.Token Lex.Plus _ _ _ : restTokens) -> do
            (actionNames, finalToks) <- parseSingleActionGroup restTokens
            return (action : actionNames, finalToks)

        (Lex.Token _ t l c : _) -> Left (parseErrorMsg "comma, plus or greater" t l c)
        [] -> Left "Unexpected end of input while parsing combo actions."

parseAction :: [Lex.Token] -> ParseResult (String, [Lex.Token])
parseAction [] = Left "Unexpected end of input while parsing combo action."
parseAction tokens = do
    (Lex.Token _ action _ _, afterAction) <- consumeToken tokens Lex.Text "input action name"
    return (action, afterAction)

parseComboAssociation :: [Lex.Token] -> ParseResult (ComboAssociation, [Lex.Token])
parseComboAssociation [] = Left "Unexpected end of input while parsing combos."
parseComboAssociation tokens = do
    (Lex.Token _ name _ _, afterName) <- consumeToken tokens Lex.Text "character name"
    (_, afterColon) <- consumeToken afterName Lex.Colon "colon"
    (Lex.Token _ combo _ _, finalTokens) <- consumeToken afterColon Lex.Text "combo name"
    return (ComboAssociation name combo, finalTokens)

parseComboAssociationList :: [Lex.Token] -> ParseResult([ComboAssociation], [Lex.Token])
parseComboAssociationList [] = Left "Unexpected end of input while parsing combos."
parseComboAssociationList tokens = do
    (comboAssoc, token) <- parseComboAssociation tokens
    case token of
        hToken@(Lex.Token t strTok l c) : nextTokens ->
            case t of
                Lex.SemiColon ->
                    return ([comboAssoc], hToken : nextTokens)
                Lex.Pipe -> do
                    (restCombos, finalTokens) <- parseComboAssociationList nextTokens
                    return (comboAssoc : restCombos, finalTokens)
                _ -> Left (parseErrorMsg "pipe or semicolon" strTok l c)
        [] -> Left "Unexpected end of input while parsing combos."

parseComboRule :: [Lex.Token] -> ParseResult (ComboRule, [Lex.Token])
parseComboRule tokens = do
    (actionGroups, nextTokens) <- parseActionsGroupList tokens
    (_, restTokens) <- consumeToken nextTokens Lex.Greater "greater than"
    (comboAssocs, finalTokens) <- parseComboAssociationList restTokens
    (_, afterSemi) <- consumeToken finalTokens Lex.SemiColon "semicolon"
    return (ComboRule actionGroups comboAssocs, afterSemi)

-- Recursively parse combo rules until RBrace token is encountered
parseComboRules :: [Lex.Token] -> ParseResult ([ComboRule], [Lex.Token])
parseComboRules [] = Left "Unexpected end of input while parsing combo rules."
parseComboRules tokens = do
    (comboRule, nextTokens) <- parseComboRule tokens
    case nextTokens of
        (Lex.Token Lex.RBrace _ _ _ : _) ->
            return ([comboRule], nextTokens)
        _ -> do
            (restComboRules, finalTokens) <- parseComboRules nextTokens
            return (comboRule : restComboRules, finalTokens)

parseCombosSection :: [Lex.Token] -> ParseResult ([ComboRule], [Lex.Token])
parseCombosSection [] = Left "Unexpected end of input while parsing combos section."
parseCombosSection (t : ts) = do
    (_, afterCombos) <- consumeToken (t : ts) Lex.Combos "combos keyword"
    (_, afterLBrace) <- consumeToken afterCombos Lex.LBrace "left brace"
    (comboRules, afterRules) <- parseComboRules afterLBrace
    (_, finalTokens) <- consumeToken afterRules Lex.RBrace "right brace"
    return (comboRules, finalTokens)

parseInputConfig :: [Lex.Token] -> ParseResult (InputConfig, [Lex.Token])
parseInputConfig tokens = do
    (keyBindings, tokensAfterKeys) <- parseKeysSection tokens
    (comboRules, restTokens) <- parseCombosSection tokensAfterKeys
    return (InputConfig keyBindings comboRules, restTokens)
