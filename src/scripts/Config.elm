module Config exposing (..)

import QueryField exposing (QueryField)


type alias Config =
    { id : String
    , label : String
    , placeholder : String
    , queryFields : List QueryField
    }
