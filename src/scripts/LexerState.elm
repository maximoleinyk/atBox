module LexerState exposing (LexerState(..))


type LexerState
    = START
    | JOIN_TERM
    | EXPRESSION
    | OPEN_PARENTHESIS_TERM
    | CLOSE_PARENTHESIS_TERM
    | OPERATOR_GROUP
    | FIELD_TERM
    | OPERATOR_TERM
    | VALUE_TERM
    | OPEN_PARENTHESIS
    | CLOSE_PARENTHESIS
