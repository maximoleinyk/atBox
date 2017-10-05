module Lexer exposing (evaluate)

import FsmState exposing (FsmType(..))
import Model exposing (Model)
import Token exposing (Token)
import Tokenizer


type LexemeType
    = Field
    | Operator
    | Value
    | Joiner
    | LeftParenthesis
    | RightParenthesis


type alias Lexeme =
    { lexemeType : LexemeType
    , value : String
    }


evaluate : List Token -> Model -> List Lexeme
evaluate tokens model =
    []
