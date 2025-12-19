
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

data ParseResult a = Error String
                   | Success a
                   deriving (Show, Eq)

parseErrorMsg :: String -> String -> Int -> Int -> String
parseErrorMsg expected found line col =
    "Expected " ++ expected ++ " instead of \"" ++ found ++ "\" at line " ++ show line ++ ", column " ++ show col

consumeToken :: [Lex.Token] -> Lex.TokenType -> String -> ParseResult (Lex.Token, [Lex.Token])
consumeToken (t:ts) expectedToken expectedStr
    | Lex.tokenType t == expectedToken = Success (t, ts)
    | otherwise = Error (parseErrorMsg expectedStr (Lex.str t) (Lex.lineNum t) (Lex.colNum t))
consumeToken [] _ expectedStr = Error ("Expected " ++ expectedStr ++ " but found end of input")

--Keys parsing

parseKeysSection :: [Lex.Token] -> ParseResult ([KeyBinding], [Lex.Token])
parseKeysSection [] = Error "Unexpected end of input while parsing keys section."
parseKeysSection (t : ts) =
    case consumeToken (t : ts) Lex.Keys "keys keyword" of
        Error err -> Error err
        Success (_, afterKeys) ->
            case consumeToken afterKeys Lex.LBrace "left brace" of
                Error err -> Error err
                Success (_, afterLBrace) ->
                    case parseKeys afterLBrace of
                        Error err -> Error err
                        Success (keyBindings, afterKeysSection) ->
                            case consumeToken afterKeysSection Lex.RBrace "right brace" of
                                Error err -> Error err
                                Success (_, finalTokens) ->
                                    Success (keyBindings, finalTokens)

parseKeys :: [Lex.Token] -> ParseResult ([KeyBinding], [Lex.Token])
parseKeys [] = Error "Unexpected end of input while parsing keys."
parseKeys (token@(Lex.Token t strTok l c) : ts)
    | t == Lex.RBrace = Success ([], token : ts)
    | t == Lex.Text =
        case parseKeysLine (token : ts) of
            Error err -> Error err
            Success (keyBinding, nextToks) ->
                case parseKeys nextToks of
                    Error err -> Error err
                    Success (restKeys, finalTokens) -> Success (keyBinding : restKeys, finalTokens)
    | otherwise = Error (parseErrorMsg "right brace" strTok l c)

parseKeysLine :: [Lex.Token] -> ParseResult (KeyBinding, [Lex.Token])
parseKeysLine tokens =
    case consumeToken tokens Lex.Text "string value" of
        Error err -> Error err
        Success (Lex.Token _ key _ _, keyRestTokens) ->
            case consumeToken keyRestTokens Lex.Equal "equal sign" of
                Error err -> Error err
                Success (_, eqRestToken) ->
                    case consumeToken eqRestToken Lex.Text "string value" of
                        Error err -> Error err
                        Success (Lex.Token _ action _ _, actRestToken) ->
                            case consumeToken actRestToken Lex.SemiColon "semicolon" of
                                Error err -> Error err
                                Success (_, finalTokens) ->
                                    Success (KeyBinding key (map toLower action), finalTokens)

-- Could use monad here with parse key line by partialy construct the return value with function chained for each token type.
-- The succession could return when fail with corresponding error message.

-- Combos Parsing

parseActionsGroupList :: [Lex.Token] -> ParseResult ([[String]], [Lex.Token])
parseActionsGroupList [] = Error "Unexpected end of input while parsing combo actions."
parseActionsGroupList tokens =
    case parseSingleActionGroup tokens of
        Error err -> Error err
        Success (actionGroup, nextToks) ->
            let sortedActionGroup = sort actionGroup
            in
            case nextToks of
                (Lex.Token Lex.Greater _ _ _ : _) -> Success ([sortedActionGroup], nextToks)
                (Lex.Token Lex.Text _ _ _ : _) ->
                    case parseActionsGroupList nextToks of
                        Error err -> Error err
                        Success (restActionGroups, finalTokens) ->
                            Success (sortedActionGroup : restActionGroups, finalTokens)
                (Lex.Token _ t l c : _) -> Error (parseErrorMsg "plus or greater" t l c)
                [] -> Error "Unexpected end of input while parsing combo actions."

parseSingleActionGroup :: [Lex.Token] -> ParseResult ([String], [Lex.Token])
parseSingleActionGroup tokens =
    case parseAction tokens of
        Error err -> Error err
        Success (action, afterAction) ->
            case afterAction of
                (Lex.Token Lex.Greater _ _ _ : _) -> Success ([action], afterAction)
                (Lex.Token Lex.Comma _ _ _ : restTokens) -> Success ([action], restTokens)
                (Lex.Token Lex.Plus _ _ _ : restTokens) ->
                    case parseSingleActionGroup restTokens of
                        Error err -> Error err
                        Success (actionNames, finalToks) -> Success (action : actionNames, finalToks)

                (Lex.Token _ t l c : _) -> Error (parseErrorMsg "comma, plus or greater" t l c)
                [] -> Error "Unexpected end of input while parsing combo actions."

parseAction :: [Lex.Token] -> ParseResult (String, [Lex.Token])
parseAction [] = Error "Unexpected end of input while parsing combo action."
parseAction tokens =
    case consumeToken tokens Lex.Text "input action name" of
        Error err -> Error err
        Success (Lex.Token _ action _ _, afterAction) -> Success (action, afterAction)

parseComboAssociation :: [Lex.Token] -> ParseResult (ComboAssociation, [Lex.Token])
parseComboAssociation [] = Error "Unexpected end of input while parsing combos."
parseComboAssociation tokens =
    case consumeToken tokens Lex.Text "character name" of
        Error err -> Error err
        Success (Lex.Token _ name _ _, afterName) ->
            case consumeToken afterName Lex.Colon "colon" of
                Error err -> Error err
                Success (_, afterColon) ->
                    case consumeToken afterColon Lex.Text "combo name" of
                        Error err -> Error err
                        Success (Lex.Token _ combo _ _, finalTokens) ->
                            Success (ComboAssociation name combo, finalTokens)

parseComboAssociationList :: [Lex.Token] -> ParseResult([ComboAssociation], [Lex.Token])
parseComboAssociationList [] = Error "Unexpected end of input while parsing combos."
parseComboAssociationList tokens =
    case parseComboAssociation tokens of
        Error err -> Error err
        Success (comboAssoc, token@(Lex.Token t strTok l c) : nextTokens) ->
            case t of
                Lex.SemiColon ->
                    Success ([comboAssoc], token : nextTokens)
                Lex.Pipe ->
                    case parseComboAssociationList nextTokens of
                        Error err -> Error err
                        Success (restCombos, finalTokens) ->
                            Success (comboAssoc : restCombos, finalTokens)
                _ -> Error (parseErrorMsg "pipe or semicolon" strTok l c)
        Success (_, []) -> Error "Unexpected end of input while parsing combos."

parseComboRule :: [Lex.Token] -> ParseResult (ComboRule, [Lex.Token])
parseComboRule tokens =
    case parseActionsGroupList tokens of
        Error err -> Error err
        Success(actionGroups, nextTokens) ->
            case consumeToken nextTokens Lex.Greater "greater than" of
                Error err -> Error err
                Success (_, restTokens) ->
                    case parseComboAssociationList restTokens of
                        Error err -> Error err
                        Success (comboAssocs, finalTokens) ->
                            case consumeToken finalTokens Lex.SemiColon "semicolon" of
                                Error err -> Error err
                                Success (_, afterSemi) ->
                                    Success (ComboRule actionGroups comboAssocs, afterSemi)

-- Recursively parse combo rules until RBrace token is encountered
parseComboRules :: [Lex.Token] -> ParseResult ([ComboRule], [Lex.Token])
parseComboRules [] = Error "Unexpected end of input while parsing combo rules."
parseComboRules tokens =
    case parseComboRule tokens of
        Error err -> Error err
        Success (comboRule, nextTokens) ->
            case nextTokens of
                (Lex.Token Lex.RBrace _ _ _ : _) -> Success ([comboRule], nextTokens)
                _ -> case parseComboRules nextTokens of
                        Error err -> Error err
                        Success (restComboRules, finalTokens) ->
                            Success (comboRule : restComboRules, finalTokens)

parseCombosSection :: [Lex.Token] -> ParseResult ([ComboRule], [Lex.Token])
parseCombosSection [] = Error "Unexpected end of input while parsing combos section."
parseCombosSection (t : ts) =
    case consumeToken (t : ts) Lex.Combos "combos keyword" of
        Error err -> Error err
        Success (_, afterCombos) ->
            case consumeToken afterCombos Lex.LBrace "left brace" of
                Error err -> Error err
                Success (_, afterLBrace) ->
                    case parseComboRules afterLBrace of
                        Error err -> Error err
                        Success (comboRules, afterRules) ->
                            case consumeToken afterRules Lex.RBrace "right brace" of
                                Error err -> Error err
                                Success (_, finalTokens) ->
                                    Success (comboRules, finalTokens)

parseInputConfig :: [Lex.Token] -> ParseResult (InputConfig, [Lex.Token])
parseInputConfig tokens =
    case parseKeysSection tokens of
        Error err -> Error err
        Success (keyBindings, tokensAfterKeys) ->
            case parseCombosSection tokensAfterKeys of
                Error err -> Error err
                Success (comboRules, restTokens) ->
                    Success (InputConfig keyBindings comboRules, restTokens)