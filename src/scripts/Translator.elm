module Translator exposing (run)

import GlobalTypes exposing (AST(..), Model, TranslatorOutput(..))


getLeafValue : AST -> String -> String
getLeafValue node operator =
    case node of
        Leaf value ->
            value

        _ ->
            ""


walk : AST -> Model -> TranslatorOutput -> TranslatorOutput
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
                            { field = getLeafValue node.left node.value
                            , operator = node.value
                            , value = getLeafValue node.right node.value
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


run : AST -> Model -> TranslatorOutput
run root model =
    let
        result =
            walk root model NoOutput
    in
    result
