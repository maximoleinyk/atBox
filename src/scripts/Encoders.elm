module Encoders
    exposing
        ( encodeAst
        , encodeFsmResponse
        , encodeLexemes
        , encodeTokens
        , encodeTranslatorOutput
        )

import GlobalTypes exposing (AST(..), FsmResponse, Lexeme, Token, TokenState, TranslatorOutput(..))
import Json.Encode exposing (Value, encode, int, list, null, object, string)


encodeAst : AST -> Value
encodeAst =
    \root ->
        case root of
            Node node ->
                object
                    [ ( "left", encodeAst node.left )
                    , ( "value", string node.value )
                    , ( "right", encodeAst node.right )
                    ]

            Leaf value ->
                encodeToString value

            Null ->
                null


encodeFsmResponse : FsmResponse -> String
encodeFsmResponse response =
    encode 2
        (object
            [ ( "tokens", encodeTokens response.tokens )
            , ( "lexemes", encodeLexemes response.lexemes )
            , ( "ast", encodeAst response.ast )
            , ( "output", encodeTranslatorOutput response.output )
            , ( "string", string response.string )
            ]
        )


encodeTokens : List Token -> Value
encodeTokens =
    \tokens ->
        let
            f =
                \t ->
                    object
                        [ ( "state", encodeToString t.state )
                        , ( "value", string t.value )
                        , ( "index", int t.index )
                        ]
        in
        list (List.map f tokens)


encodeLexemes : List Lexeme -> Value
encodeLexemes =
    \lexemes ->
        let
            f =
                \l ->
                    object
                        [ ( "lexemeType", encodeToString l.lexemeType )
                        , ( "value", string l.value )
                        , ( "index", int l.index )
                        ]
        in
        list (List.map f lexemes)


encodeTranslatorOutputs : List TranslatorOutput -> List Value
encodeTranslatorOutputs =
    \list ->
        case list of
            [] ->
                []

            next :: rest ->
                [ encodeTranslatorOutput next ] ++ encodeTranslatorOutputs rest


encodeTranslatorOutput : TranslatorOutput -> Value
encodeTranslatorOutput =
    \output ->
        case output of
            AndOutput output ->
                object
                    [ ( "and", list (encodeTranslatorOutputs output.and) ) ]

            OrOutput output ->
                object
                    [ ( "or", list (encodeTranslatorOutputs output.or) ) ]

            EndOutput output ->
                object
                    [ ( "field", string output.field )
                    , ( "operator", string output.operator )
                    , ( "value", string output.value )
                    ]

            NoOutput ->
                null


encodeToString =
    \encodeSimpleType -> string (toString encodeSimpleType)
