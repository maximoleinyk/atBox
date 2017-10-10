module Config exposing (Config)

import QueryField exposing (QueryField)


type alias Config =
    { id : String
    , label : String
    , placeholder : String
    , queryFields : List QueryField
    , value : String
    }
