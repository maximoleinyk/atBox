module Actions exposing (..)


type Msg
    = Parse String
    | EnterKeyPressed
    | ArrowUpPressed
    | ArrowDownPressed
    | TabKeyPressed
    | GetCaretPosition
    | UpdateCaretIndex Int
    | ArrowLeftPressed
    | ArrowRightPressed
