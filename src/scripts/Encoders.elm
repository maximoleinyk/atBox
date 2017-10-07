module Encoders exposing (encodeLexemeType, encodeLexemes, encodeParsedToken, encodeState, encodeTokens)

import Json.Encode exposing (Value, int, list, object, string)
import Lexer exposing (Lexeme, LexemeType)
import Tokenizer exposing (ParsedToken, Token, TokenState)


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
                    [ ( "lexemeType", encodeLexemeType l.lexemeType )
                    , ( "value", string l.value )
                    ]
    in
    list (List.map f lexemes)


encodeLexemeType : LexemeType -> Json.Encode.Value
encodeLexemeType =
    \lexemeType -> string (toString lexemeType)
