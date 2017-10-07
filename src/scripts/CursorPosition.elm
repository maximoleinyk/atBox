module CursorPosition exposing (CursorPosition(..))


type CursorPosition
    = NoContext
    | AfterAtSymbol
    | AfterAtField
    | AfterOperatorAndValue
