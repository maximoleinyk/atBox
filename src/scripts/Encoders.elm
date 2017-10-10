module Encoders
    exposing
        ( encodeAst
        , encodeFsmResponse
        , encodeLexemes
        , encodeParsedToken
        , encodeState
        , encodeTokens
        , encodeTranslatorOutput
        )

import FsmResponse exposing (FsmResponse)
import Json.Encode exposing (Value, encode, int, list, null, object, string)
import Lexer exposing (Lexeme, LexemeType)
import Parser exposing (AST(..))
import Tokenizer exposing (ParsedToken, Token, TokenState)
import Translator exposing (Output(..))


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
                encodeSimpleType value

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
            ]
        )


encodeState : TokenState -> Value
encodeState =
    \state -> string (toString state)


encodeParsedToken : ParsedToken -> Value
encodeParsedToken =
    \parsedToken ->
        object
            [ ( "string", string parsedToken.string )
            , ( "length", int parsedToken.length )
            ]


encodeTokens : List Token -> Value
encodeTokens =
    \tokens ->
        let
            f =
                \t ->
                    object
                        [ ( "state", encodeState t.state )
                        , ( "parsedToken", encodeParsedToken t.parsedToken )
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
                        [ ( "lexemeType", encodeSimpleType l.lexemeType )
                        , ( "value", string l.value )
                        ]
        in
        list (List.map f lexemes)


encodeList : List Output -> List Value
encodeList =
    \list ->
        case list of
            [] ->
                []

            next :: rest ->
                [ encodeTranslatorOutput next ] ++ encodeList rest


encodeTranslatorOutput : Output -> Value
encodeTranslatorOutput =
    \output ->
        case output of
            AndOutput output ->
                object
                    [ ( "and", list (encodeList output.and) ) ]

            OrOutput output ->
                object
                    [ ( "or", list (encodeList output.or) ) ]

            EndOutput output ->
                object
                    [ ( "field", string output.field )
                    , ( "operator", string output.operator )
                    , ( "value", string output.value )
                    ]

            NoOutput ->
                null


encodeSimpleType =
    \encodeSimpleType -> string (toString encodeSimpleType)
