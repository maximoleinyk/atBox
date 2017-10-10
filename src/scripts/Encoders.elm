module Encoders
    exposing
        ( encodeAst
        , encodeFsmResponse
        , encodeLexemes
        , encodeParsedToken
        , encodeState
        , encodeTokens
        )

import FsmResponse exposing (FsmResponse)
import Json.Encode exposing (Value, int, list, null, object, string)
import Lexer exposing (Lexeme, LexemeType)
import Parser exposing (AST(..))
import Tokenizer exposing (ParsedToken, Token, TokenState)


encodeAst : AST -> Value
encodeAst root =
    case root of
        Node node ->
            object
                [ ( "left", encodeAst node.left )
                , ( "value", string node.value )
                , ( "right", encodeAst node.right )
                ]

        Leaf value ->
            encodeSimpleType value

        Nil ->
            null


encodeFsmResponse : FsmResponse -> String
encodeFsmResponse response =
    Json.Encode.encode 2
        (object
            [ ( "tokens", encodeTokens response.tokens )
            , ( "lexemes", encodeLexemes response.lexemes )
            , ( "ast", encodeAst response.ast )
            ]
        )


encodeState : TokenState -> Value
encodeState =
    \state -> string (toString state)


encodeParsedToken : ParsedToken -> Value
encodeParsedToken parsedToken =
    object
        [ ( "string", string parsedToken.string )
        , ( "length", int parsedToken.length )
        ]


encodeTokens : List Token -> Json.Encode.Value
encodeTokens tokens =
    let
        f =
            \t ->
                object
                    [ ( "state", encodeState t.state )
                    , ( "parsedToken", encodeParsedToken t.parsedToken )
                    ]
    in
    list (List.map f tokens)


encodeLexemes : List Lexeme -> Json.Encode.Value
encodeLexemes lexemes =
    let
        f =
            \l ->
                object
                    [ ( "lexemeType", encodeSimpleType l.lexemeType )
                    , ( "value", string l.value )
                    ]
    in
    list (List.map f lexemes)


encodeSimpleType =
    \encodeSimpleType -> string (toString encodeSimpleType)
