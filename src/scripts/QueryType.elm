module QueryType exposing (..)

import Json.Decode exposing (Decoder, succeed)


type QueryType
    = Str


queryTypeDecoder : Decoder QueryType
queryTypeDecoder =
    succeed Str
