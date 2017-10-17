module Encoders
    exposing
        ( encodeAst
        , encodeFsmResponse
        , encodeLexemes
        , encodeTokens
        , encodeTranslatorOutput
        )

import GlobalTypes exposing (AST(..), FsmResponse, Lexeme, OutputOperatorType(..), OutputValueType(..), Token, TokenState, TranslatorOutput(..), TranslatorOutputValueType(..))
import Json.Encode exposing (Value, encode, int, list, null, object, string)


encodeAst : AST -> Value
encodeAst =
    \root ->
        case root of
            Node node ->
                object
                    [ ( "left", encodeAst node.left )
                    , ( "value", encodeNodeValue node.value )
                    , ( "right", encodeAst node.right )
                    ]

            Leaf value ->
                encodeToString value

            Null ->
                null


encodeNodeValue : OutputOperatorType -> Value
encodeNodeValue outputOperatorValue =
    let
        convertedValue =
            case outputOperatorValue of
                ContainsOperatorType ->
                    "like"

                IsOperatorType ->
                    "="

                IsNotOperatorType ->
                    "!="

                IsEitherOperatorType ->
                    "in"

                IsNeitherOperatorType ->
                    "not in"

                IsInOperatorType ->
                    "in"

                IsNotInOperatorType ->
                    "not in"

                OrOperatorType ->
                    "||"

                AndOperatorType ->
                    "&&"

                NoOutputType ->
                    ""
    in
    string convertedValue


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
                        [ ( "state", encodeToString l.state )
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


encodeTranslatorOutputValue : TranslatorOutputValueType -> Value
encodeTranslatorOutputValue value =
    case value of
        Single v ->
            string v

        Multiple l ->
            list (List.map (\i -> string i) l)

        None ->
            null


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
                    , ( "value", encodeTranslatorOutputValue output.value )
                    ]

            NoOutput ->
                null


encodeToString =
    \encodeSimpleType -> string (toString encodeSimpleType)
