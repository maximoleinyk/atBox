module SyntaxHighlighter exposing (run)

import GlobalTypes exposing (Token, TokenState(AndTerm, IsEitherTerm, IsInTerm, IsNeitherTerm, IsTerm, KeywordTerm, OrTerm, UnknownKeywordTerm, WordTerm))


highlightWords : List Token -> String -> String
highlightWords remainingTokens string =
    case remainingTokens of
        [] ->
            string

        next :: rest ->
            let
                exists =
                    \item ->
                        item == next.state

                isKeywordTerm =
                    List.any exists
                        [ KeywordTerm
                        , UnknownKeywordTerm
                        ]

                isBoldTerm =
                    List.any exists
                        [ WordTerm
                        ]

                isItalicTerm =
                    List.any exists
                        [ IsTerm
                        , IsInTerm
                        , IsEitherTerm
                        , IsNeitherTerm
                        ]

                newString =
                    if isBoldTerm then
                        "<b>" ++ next.value ++ "</b>"
                    else if isKeywordTerm then
                        "<b class='grayed-out'>" ++ next.value ++ "</b>"
                    else if isItalicTerm then
                        "<i>" ++ next.value ++ "</i>"
                    else
                        next.value
            in
            highlightWords rest (string ++ newString)


run : List Token -> String
run tokens =
    let
        result =
            highlightWords tokens ""
    in
    result
