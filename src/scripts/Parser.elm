module Parser exposing (AST(..), run)

import Lexer exposing (Lexeme, LexemeType(Field, Joiner, LeftParenthesis, Operator, RightParenthesis, Value))
import Model exposing (Model)
import OperatorType exposing (OperatorType)


type Term
    = Expression (List Term)
    | Item Lexeme
    | AndJoiner
    | OrJoiner
    | LParenthesis
    | RParenthesis


type AST
    = Node
        { left : AST
        , value : String
        , right : AST
        }
    | Leaf String
    | Nil


processRightParenthesis : Lexeme -> Model -> List Term -> List Lexeme -> List Term
processRightParenthesis nextLexeme model stack restLexemes =
    let
        extractItems : List Term -> List Term -> ( List Term, List Term )
        extractItems =
            \s memo ->
                case s of
                    [] ->
                        ( s, memo )

                    nextItem :: restStack ->
                        case nextItem of
                            LParenthesis ->
                                ( restStack, memo )

                            _ ->
                                let
                                    newMemo =
                                        [ nextItem ] ++ memo
                                in
                                extractItems restStack newMemo

        ( expressionStackOnly, extractedItems ) =
            extractItems (List.reverse stack) []

        newStack =
            if List.length extractedItems == 1 then
                List.reverse expressionStackOnly ++ extractedItems
            else
                List.reverse expressionStackOnly ++ [ Expression extractedItems ]
    in
    processExpression nextLexeme model newStack restLexemes


processLeftParenthesis : Lexeme -> Model -> List Term -> List Lexeme -> List Term
processLeftParenthesis nextLexeme model stack restLexemes =
    buildExpressionTree restLexemes model (stack ++ [ LParenthesis ])


processField : Lexeme -> Model -> List Term -> List Lexeme -> List Term
processField nextLexeme model stack restLexemes =
    let
        newStack =
            stack ++ [ Item nextLexeme ]
    in
    buildExpressionTree restLexemes model newStack


processOperator : Lexeme -> Model -> List Term -> List Lexeme -> List Term
processOperator nextLexeme model stack restLexemes =
    let
        newStack =
            stack ++ [ Item nextLexeme ]
    in
    buildExpressionTree restLexemes model newStack


processExpression : Lexeme -> Model -> List Term -> List Lexeme -> List Term
processExpression nextLexeme model stack restLexemes =
    let
        extractItems : List Term -> List Term -> ( List Term, List Term )
        extractItems =
            \s memo ->
                case s of
                    [] ->
                        ( s, memo )

                    nextItem :: restStack ->
                        case nextItem of
                            LParenthesis ->
                                ( s, memo )

                            OrJoiner ->
                                ( s, memo )

                            _ ->
                                extractItems restStack ([ nextItem ] ++ memo)

        ( expressionStackOnly, extractedItems ) =
            extractItems (List.reverse stack) []

        newStack =
            if List.length extractedItems == 1 then
                List.reverse expressionStackOnly ++ extractedItems
            else
                List.reverse expressionStackOnly ++ [ Expression extractedItems ]

        optimizedStack =
            if List.length newStack == 1 then
                optimizeStack newStack []
            else
                newStack
    in
    buildExpressionTree restLexemes model optimizedStack


processValue : Lexeme -> Model -> List Term -> List Lexeme -> List Term
processValue nextLexeme model stack restLexemes =
    let
        extractItems : List Term -> List Term -> ( List Term, List Term )
        extractItems =
            \s memo ->
                case s of
                    [] ->
                        ( s, memo )

                    nextItem :: restStack ->
                        case nextItem of
                            Item l ->
                                extractItems restStack ([ nextItem ] ++ memo)

                            _ ->
                                ( s, memo )

        reversedStack =
            List.reverse (stack ++ [ Item nextLexeme ])

        ( expressionStackOnly, extractedItems ) =
            extractItems reversedStack []

        reversedExpressionStackOnly =
            List.reverse expressionStackOnly

        newStack =
            reversedExpressionStackOnly ++ [ Expression extractedItems ]
    in
    processExpression nextLexeme model newStack restLexemes


processJoiner : Lexeme -> Model -> List Term -> List Lexeme -> List Term
processJoiner nextLexeme model stack restLexemes =
    let
        joinerValue =
            String.trim nextLexeme.value
                |> String.toLower

        termType =
            if joinerValue == "or" then
                OrJoiner
            else
                AndJoiner

        extractItems : List Term -> List Term -> ( List Term, List Term )
        extractItems =
            \s memo ->
                case s of
                    [] ->
                        ( s, memo )

                    nextItem :: restStack ->
                        case nextItem of
                            LParenthesis ->
                                ( s, memo )

                            OrJoiner ->
                                ( s, memo )

                            _ ->
                                let
                                    newMemo =
                                        [ nextItem ] ++ memo
                                in
                                extractItems restStack newMemo

        ( expressionStackOnly, extractedItems ) =
            extractItems (List.reverse stack) []

        newStack =
            if List.length extractedItems == 1 then
                List.reverse expressionStackOnly ++ extractedItems ++ [ termType ]
            else
                List.reverse expressionStackOnly ++ [ Expression extractedItems ] ++ [ termType ]
    in
    buildExpressionTree restLexemes model newStack


buildExpressionTree : List Lexeme -> Model -> List Term -> List Term
buildExpressionTree lexemes model stack =
    case lexemes of
        [] ->
            stack

        nextLexeme :: restLexemes ->
            case nextLexeme.lexemeType of
                Field ->
                    processField nextLexeme model stack restLexemes

                Operator operatorType ->
                    processOperator nextLexeme model stack restLexemes

                Value ->
                    processValue nextLexeme model stack restLexemes

                Joiner ->
                    processJoiner nextLexeme model stack restLexemes

                LeftParenthesis ->
                    processLeftParenthesis nextLexeme model stack restLexemes

                RightParenthesis ->
                    processRightParenthesis nextLexeme model stack restLexemes


optimizeStack : List Term -> List Term -> List Term
optimizeStack stack memo =
    case stack of
        [] ->
            []

        [ x ] ->
            [ x ]

        next :: rest ->
            case next of
                Expression items ->
                    let
                        extractItems : List Term -> List Term -> ( List Term, List Term )
                        extractItems =
                            \s memo ->
                                case s of
                                    [] ->
                                        ( s, memo )

                                    nextItem :: restStack ->
                                        case nextItem of
                                            Expression items ->
                                                if List.length memo < 1 then
                                                    extractItems restStack ([ nextItem ] ++ memo)
                                                else
                                                    ( restStack, [ nextItem ] ++ memo )

                                            _ ->
                                                extractItems restStack ([ nextItem ] ++ memo)

                        ( expressionStackOnly, extractedItems ) =
                            extractItems (List.reverse stack) []

                        reversedExpressionStackOnly =
                            List.reverse expressionStackOnly

                        newStack =
                            reversedExpressionStackOnly ++ [ Expression extractedItems ]
                    in
                    optimizeStack newStack []

                OrJoiner ->
                    optimizeStack rest memo ++ [ next ]

                _ ->
                    -- should be unreachable because of [Expression OR Expression OR Expression] only
                    rest


convertTermToString term =
    case term of
        Expression terms ->
            toString terms

        Item lexeme ->
            lexeme.value

        AndJoiner ->
            "and"

        OrJoiner ->
            "or"

        LParenthesis ->
            "("

        RParenthesis ->
            ")"


traverseTree : List Term -> AST
traverseTree stack =
    case stack of
        [] ->
            Nil

        [ x ] ->
            case x of
                Expression items ->
                    traverseTree items

                _ ->
                    Debug.log "unreachable" Nil

        x :: y :: [] ->
            traverseTree [ x ]

        x :: y :: z :: _ ->
            if y == AndJoiner || y == OrJoiner then
                Node
                    { left = traverseTree [ x ]
                    , value = convertTermToString y
                    , right = traverseTree [ z ]
                    }
            else
                Node
                    { left = Leaf (convertTermToString x)
                    , value = convertTermToString y
                    , right = Leaf (convertTermToString z)
                    }


run : List Lexeme -> Model -> AST
run lexemes model =
    let
        stack =
            buildExpressionTree lexemes model []

        singleRoot =
            optimizeStack stack []

        result =
            traverseTree singleRoot

        a =
            Debug.log "stack" result
    in
    result
