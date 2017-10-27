module Translator exposing (run)

import GlobalTypes exposing (AST(..), Model, OutputOperatorType(..), OutputValueType(..), TranslatorOutput(..), TranslatorOutputValueType(Multiple, None, Single))
import Utils


getOutputOperatorType : OutputOperatorType -> String
getOutputOperatorType t =
    case t of
        ContainsOperatorType ->
            "contins"

        IsOperatorType ->
            "=="

        IsNotOperatorType ->
            "!="

        IsEitherOperatorType ->
            "in"

        IsNeitherOperatorType ->
            "not in"

        IsInOperatorType ->
            "in"

        IsNotInOperatorType ->
            "not in"

        OrOperatorType ->
            "||"

        AndOperatorType ->
            "&&"

        NoOutputType ->
            ""


getKeywordValue : AST -> String
getKeywordValue node =
    case node of
        Leaf outputValueType ->
            case outputValueType of
                SingleValue string ->
                    string

                _ ->
                    ""

        _ ->
            ""


getValue : AST -> TranslatorOutputValueType
getValue node =
    case node of
        Leaf outputValueType ->
            case outputValueType of
                SingleValue value ->
                    Single (String.trim value)

                MultipleValues list ->
                    Multiple (List.map (\i -> String.trim i) list)

                NoValue ->
                    None

        _ ->
            None


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
                isLeaf =
                    node.value /= AndOperatorType && node.value /= OrOperatorType
            in
            if isLeaf then
                let
                    result =
                        EndOutput
                            { field = getKeywordValue node.left
                            , operator = getOutputOperatorType node.value
                            , value = getValue node.right
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
                if node.value == OrOperatorType then
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
