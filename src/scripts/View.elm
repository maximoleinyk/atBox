module View exposing (view)

import Dict exposing (Dict)
import GlobalTypes exposing (CursorContext(..), Model, Msg(..))
import Html exposing (Attribute, Html, a, div, input, label, li, text)
import Html.Attributes exposing (class, for, href, id, placeholder, tabindex, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onFocus, onInput, onMouseDown, onWithOptions, targetValue)
import Html.Keyed exposing (ul)
import Json.Decode exposing (andThen, at, fail, map, string, succeed)
import Regex exposing (HowMany(All, AtMost))
import String exposing (contains, toLower)
import Utils


onKeyUpGetCaretPosition : Attribute Msg
onKeyUpGetCaretPosition =
    on "keyup" (succeed GetCursorPosition)


onPreventDefaultClick : Msg -> Attribute Msg
onPreventDefaultClick action =
    let
        options =
            Html.Events.Options False True
    in
    onWithOptions "click" options (succeed action)


onKeyDown : Model -> Attribute Msg
onKeyDown model =
    let
        options =
            Html.Events.Options False False

        handleKeyPress code =
            case code of
                13 ->
                    succeed EnterKeyPressed

                9 ->
                    succeed TabKeyPressed

                37 ->
                    succeed ArrowLeftPressed

                38 ->
                    succeed ArrowUpPressed

                39 ->
                    succeed ArrowRightPressed

                40 ->
                    if model.context /= NoContext then
                        succeed ArrowDownPressed
                    else
                        fail "do nothing"

                _ ->
                    fail "unsupported"
    in
    onWithOptions "keydown" options (andThen handleKeyPress keyCode)


renderLabel : Model -> Html Msg
renderLabel model =
    let
        hasLabel =
            String.length model.label > 0
    in
    if hasLabel then
        label [ for model.id ] [ text model.label ]
    else
        text ""


view : Model -> Html Msg
view model =
    div [ class "at-box" ]
        [ div [ class "at-box-group" ]
            [ renderLabel model
            , input
                [ type_ "text"
                , id model.id
                , value model.value
                , placeholder model.placeholder
                , onInput UpdateValue
                , onKeyUpGetCaretPosition
                , onMouseDown GetCursorPosition
                , onFocus Focus
                , onBlur Blur
                , onKeyDown model
                ]
                []
            ]
        , div [ class "at-box-list" ]
            [ getContextHtml model ]
        ]


renderDropdown : Dict String String -> String -> (String -> String) -> (String -> String -> String) -> Html Msg
renderDropdown dict query formatQuery obtainValue =
    let
        simplifyValue =
            \value ->
                value
                    |> String.trim
                    |> String.toLower

        renderA =
            \key value ->
                let
                    action =
                        SelectHighlightedValue
                            { itemKey = key
                            , replacementValue = obtainValue query value
                            }
                in
                a [ href "#", onPreventDefaultClick action ] [ text value ]

        renderLi =
            \key value ->
                ( key, li [] [ renderA key value ] )

        startsWith =
            \key value ->
                let
                    queryString =
                        simplifyValue (formatQuery query)

                    matchingItem =
                        simplifyValue value

                    result =
                        String.startsWith queryString matchingItem
                in
                result

        filteredItems =
            Dict.filter startsWith dict

        liItems =
            filteredItems
                |> Dict.map renderLi
                |> Dict.values

        html =
            ul [ class "at-box-suggest-list" ] liItems

        size =
            Dict.size filteredItems

        nothing =
            text ""
    in
    if size == 0 then
        nothing
    else if size == 1 then
        let
            head =
                List.head (Dict.values filteredItems)
        in
        case head of
            Nothing ->
                nothing

            Just value ->
                if simplifyValue value == query then
                    nothing
                else
                    html
    else
        html


getContextHtml : Model -> Html Msg
getContextHtml model =
    if not model.focused then
        text ""
    else
        let
            asIs =
                \string -> String.trim string

            obtainValue =
                \q dropdownItemText ->
                    let
                        replacedString =
                            String.trim (Utils.replace "\\S+" " " q)

                        totalAmountOfTypedCharacters =
                            String.length replacedString

                        dropdownItemTextLength =
                            String.length dropdownItemText

                        secondPartOfTheString =
                            String.slice totalAmountOfTypedCharacters dropdownItemTextLength dropdownItemText
                    in
                    secondPartOfTheString ++ " "
        in
        case model.context of
            NoContext ->
                text ""

            -- Ex: find a person whose @____
            KeywordContext dict query ->
                let
                    obtainValue =
                        \queryString dropdownItemText ->
                            let
                                replacedString =
                                    String.trim (Utils.replace model.keywordDelimiter "" queryString)

                                totalAmountOfTypedCharacters =
                                    String.length replacedString

                                dropdownItemTextLength =
                                    String.length dropdownItemText

                                secondPartOfTheString =
                                    String.slice totalAmountOfTypedCharacters dropdownItemTextLength dropdownItemText
                            in
                            if List.length (String.split model.keywordDelimiter query) > 1 then
                                secondPartOfTheString ++ " "
                            else
                                model.keywordDelimiter ++ secondPartOfTheString ++ " "

                    formatQuery =
                        \q -> String.dropLeft 1 q
                in
                renderDropdown dict query formatQuery obtainValue

            -- Ex: find a person whose @name ____
            OperatorContext dict query ->
                renderDropdown dict query asIs obtainValue

            -- Ex: find a person whose @name is _____
            ValueContext dict query ->
                renderDropdown dict query asIs obtainValue

            -- Ex: find a person whose @name is either Max ____
            -- Ex: find a person whose @name is in (Max____
            ValueSeparatorContext valueType dict query ->
                renderDropdown dict query asIs obtainValue

            -- Ex: find a person whose @
            JoinerContext dict query ->
                renderDropdown dict query asIs obtainValue

            FreeContext dict query ->
                renderDropdown dict query asIs obtainValue
