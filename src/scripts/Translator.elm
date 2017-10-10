module Translator exposing (Output(..), run)

import Model exposing (Model)
import Parser exposing (AST(..))


type Output
    = AndOutput { and : List Output }
    | OrOutput { or : List Output }
    | EndOutput
        { field : String
        , operator : String
        , value : String
        }
    | NoOutput


getLeafValue : AST -> String
getLeafValue node =
    case node of
        Leaf value ->
            value

        _ ->
            ""


walk : AST -> Model -> Output -> Output
walk root model output =
    case root of
        Null ->
            output

        Leaf value ->
            -- !unreachable code - would be parsed one level higher
            output

        Node node ->
            let
                isAnd =
                    node.value == "and"

                isOr =
                    node.value == "or"

                isLeaf =
                    not isAnd && not isOr
            in
            if isLeaf then
                let
                    result =
                        EndOutput
                            { field = getLeafValue node.left
                            , operator = node.value
                            , value = getLeafValue node.right
                            }
                in
                case output of
                    EndOutput output ->
                        result

                    AndOutput output ->
                        AndOutput { and = output.and ++ [ result ] }

                    OrOutput output ->
                        OrOutput { or = output.or ++ [ result ] }

                    NoOutput ->
                        result
            else
                let
                    result =
                        [ walk node.left model NoOutput ] ++ [ walk node.right model NoOutput ]
                in
                if isOr then
                    OrOutput { or = result }
                else
                    AndOutput { and = result }


run : AST -> Model -> Output
run root model =
    let
        result =
            walk root model NoOutput
    in
    result
