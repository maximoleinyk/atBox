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
        ]
        NoContext
        [ Operator "or" "or" True
        , Operator "and" "and" True
        , Operator "is" "is" False
        , Operator "is not" "is not" False
        , Operator "is either" "is either" False
        , Operator "is neither" "is neither" False
        , Operator "is in" "is in" False
        , Operator "is not in" "is not in" False
        ]
        ""
        ""
        "@"
