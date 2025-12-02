module Parsing.AlityParser where

import qualified Parsing.AlityLexer as Lex

data ComboAssociation = ComboAssociation { characterName :: String
                                         , comboName :: String
                                         } deriving (Show, Eq)

data ComboRule = ComboRule { actions :: [[String]]
                           , comboAssociations :: [ComboAssociation]
                           } deriving (Show, Eq)

data InputConfig = InputConfig { keys :: [KeyBinding]
                               , combos :: [ComboRule]
                               } deriving (Show, Eq)

data KeyBinding = KeyBinding { keyName :: String
                             , actionName :: String
                             } deriving (Show, Eq)

--Keys parsing

-- Keys section start with Keys token following by LBrace token, and end with RBrace token
parseKeysSection :: [Lex.Token] -> ([KeyBinding], [Lex.Token])
parseKeysSection tokens =
    case tokens of
        (Lex.Keys : Lex.LBrace : xs) ->
            let (keyBindings, restTokens) = parseKeys xs
            in case restTokens of
                (Lex.RBrace : ys) -> (keyBindings, ys)
                _                 -> (keyBindings, restTokens)

        _ -> ([], tokens)

parseKeys :: [Lex.Token] -> ([KeyBinding], [Lex.Token])
parseKeys [] = ([], [])
parseKeys tokens =
    case parseKeysLine tokens of
        (Nothing, nextToks) -> 
            ([], nextToks)

        (Just keyBinding, nextToks) ->
            let (restKeys, finalTokens) = parseKeys nextToks
            in (keyBinding : restKeys, finalTokens)

parseKeysLine :: [Lex.Token] -> (Maybe KeyBinding, [Lex.Token])
parseKeysLine (k:e:a:c:xs) =
    case (k, e, a, c) of
        (Lex.Text key, Lex.Equal, Lex.Text action, Lex.SemiColon) ->
            (Just (KeyBinding  key action), xs)
        (_, _, _, _) ->
            (Nothing, k:e:a:c:xs)

parseKeysLine tokens = (Nothing, tokens)

-- Combos Parsing

-- Actions list is a series of Text tokens, separated by Comma tokens, and can be joined when separated by Plus tokens
parseActionsList :: [Lex.Token] -> ([[String]], [Lex.Token])
parseActionsList [] = ([], [])
parseActionsList tokens =
    case parseSingleActionGroup tokens of
        (Nothing, nextToks) ->
            ([], nextToks)

        (Just actionGroup, nextToks) ->
            let (restActionGroups, finalTokens) = parseActionsList nextToks
            in (actionGroup : restActionGroups, finalTokens)

parseSingleActionGroup :: [Lex.Token] -> (Maybe [String], [Lex.Token])
parseSingleActionGroup tokens =
    case parseAction tokens of
        (Nothing, nextTokens) -> (Nothing, nextTokens)

        (Just action, nextTokens) ->
            case nextTokens of
                (Lex.Plus : restTokens) ->
                    let nextAction = parseSingleActionGroup restTokens
                    in case nextAction of
                        (Nothing, finalToks) -> (Just [action], finalToks)
                        (Just actionNames, finalToks) -> (Just (action : actionNames), finalToks)

                (Lex.Comma : restTokens) -> (Just [action], restTokens)
                _                        -> (Just [action], nextTokens)

parseAction :: [Lex.Token] -> (Maybe String, [Lex.Token])
parseAction (Lex.Text action : xs) = (Just action, xs)
parseAction tokens                     = (Nothing, tokens)

-- A combo association is a Text token followed by Colon token and another Text token
parseComboAssociation :: [Lex.Token] -> (Maybe ComboAssociation, [Lex.Token])
parseComboAssociation [] = (Nothing, [])
parseComboAssociation (Lex.Text name : Lex.Colon : Lex.Text combo : xs) =
    (Just (ComboAssociation name combo), xs)

parseComboAssociation tokens = (Nothing, tokens)

-- Combo association list is a series of combo associations separated by Pipe tokens
parseComboAssociationList :: [Lex.Token] -> ([ComboAssociation], [Lex.Token])
parseComboAssociationList [] = ([], [])
parseComboAssociationList tokens =
    case parseComboAssociation tokens of
        (Nothing, nextTokens) -> ([], nextTokens)

        (Just comboAssoc, nextTokens) ->
            case nextTokens of
                (Lex.Pipe : restTokens) ->
                    let (restCombos, finalTokens) = parseComboAssociationList restTokens
                    in (comboAssoc : restCombos, finalTokens)

                _ -> ([comboAssoc], nextTokens)

-- A combo rule consists of an actions ad a combo association list separated by a Greater token, and end with a SemiColon token
parseComboRule :: [Lex.Token] -> (Maybe ComboRule, [Lex.Token])
parseComboRule tokens =
    case parseActionsList tokens of
        ([], nextTokens) -> (Nothing, nextTokens)

        (actionGroups, nextTokens) ->
            case nextTokens of
                (Lex.Greater : restTokens) ->
                    let (comboAssocs, finalTokens) = parseComboAssociationList restTokens
                    in case finalTokens of
                        (Lex.SemiColon : afterSemi) ->
                            (Just (ComboRule actionGroups comboAssocs), afterSemi)

                        _ -> (Nothing, finalTokens) -- TODO better error handling

                _ -> (Nothing, nextTokens) -- TODO better error handling

-- Recursively parse combo rules until RBrace token is encountered
parseComboRules :: [Lex.Token] -> ([ComboRule], [Lex.Token])
parseComboRules [] = ([], [])
parseComboRules tokens =
    case parseComboRule tokens of
        (Nothing, nextTokens) -> ([], nextTokens)
        (Just comboRule, nextTokens) ->
            let (restComboRules, finalTokens) = parseComboRules nextTokens
            in (comboRule : restComboRules, finalTokens)

-- A combo secttion starts with Combos token followed by LBrace token, and ends with RBrace token, and contains a series of combo rules
parseCombosSection :: [Lex.Token] -> ([ComboRule], [Lex.Token])
parseCombosSection tokens =
    case tokens of
        (Lex.Combos : Lex.LBrace : xs) ->
            let (comboRules, restTokens) = parseComboRules xs
            in case restTokens of
                (Lex.RBrace : ys) -> (comboRules, ys)
                _                 -> (comboRules, restTokens) -- TODO better error handling

        _ -> ([], tokens)


parseInputConfig :: [Lex.Token] -> (InputConfig, [Lex.Token])
parseInputConfig tokens =
    let (keyBindings, tokensAfterKeys) = parseKeysSection tokens
        (comboRules, restTokens)       = parseCombosSection tokensAfterKeys
    in (InputConfig keyBindings comboRules, restTokens)