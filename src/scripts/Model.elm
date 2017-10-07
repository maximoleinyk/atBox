module Model exposing (..)

import CursorPosition exposing (CursorPosition)
import Operator exposing (Operator)
import QueryField exposing (QueryField)


type alias Model =
    { id : String
    , label : String
    , placeholder : String
    , value : String
    , queryFields : List QueryField
    , cursorPosition : CursorPosition
    , operators : List Operator
    , currentToken : String
    , selectedItem : String
    , keywordDelimiter : String
    , cursorIndex : Int
    }
