module Init exposing (init)

import GlobalTypes exposing (Config, CursorContext(NoContext), Model, Msg(Init))
import Update exposing (update)


init : Config -> ( Model, Cmd Msg )
init config =
    update Init
        (Model
            config.id
            config.label
            config.placeholder
            config.value
            config.queryFields
            NoContext
            ""
            "@"
            0
            False
            config.autoSuggest
        )
