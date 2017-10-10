module MockModel exposing (getDefaultModel)

import CursorPosition exposing (CursorPosition(NoContext))
import Model exposing (Model)
import Operator exposing (Operator)
import QueryField exposing (QueryField)


getDefaultModel : Model
getDefaultModel =
    Model
        "id"
        "label"
        "placeholder"
        ""
        [ QueryField "name" "name" "string"
        , QueryField "surname" "surname" "string"
        , QueryField "forename" "forename" "string"
        , QueryField "age" "age" "string"
        ]
        NoContext
        [ Operator "or" "or"
        , Operator "and" "and"
        , Operator "is" "is"
        , Operator "is not" "is not"
        , Operator "is either" "is either"
        , Operator "is neither" "is neither"
        , Operator "is in" "is in"
        , Operator "is not in" "is not in"
        ]
        ""
        "@"
        0
