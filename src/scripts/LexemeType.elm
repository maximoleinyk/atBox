module LexemeType exposing (LexemeType(..))

import OperatorType exposing (OperatorType)


type LexemeType
    = Field
    | Operator OperatorType
    | Value
    | Joiner
    | LeftParenthesis
    | RightParenthesis
