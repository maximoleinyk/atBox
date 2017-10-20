module MockModel exposing (getDefaultModel)

import GlobalTypes exposing (CursorContext(NoContext), Model, QueryField)


getDefaultModel : Model
getDefaultModel =
    Model
        "id"
        "label"
        "placeholder"
        ""
        [ QueryField "name" "name" "string" []
        , QueryField "surname" "surname" "string" []
        , QueryField "forename" "forename" "string" []
        , QueryField "age" "age" "string" []
        ]
        NoContext
        ""
        "@"
        0
        False
        False
