module Fsm exposing (getPossibleStates)

import FsmState exposing (FsmState(..))


getPossibleStates : FsmState -> List FsmState
getPossibleStates state =
    case state of
        Start ->
            [ Statement, Criteria ]

        Statement ->
            [ SpaceTerm, WordTerm, Statement ]

        Criteria ->
            [ Criterion, Criteria ]

        Criterion ->
            [ OpenParenthesisTerm, SpaceTerm, OperatorGroup, SpaceTerm, CloseParenthesisTerm, SpaceTerm, Conjunction ]

        OperatorGroup ->
            [ KeywordTerm, SpaceTerm, Operator, SpaceTerm, Value ]

        Operator ->
            [ IsOperator ]

        IsOperator ->
            [ IsTerm, SpaceTerm, IsSubOperator ]

        IsSubOperator ->
            [ EitherOrOperator, NeitherNorOperator, NotTerm, SpaceTerm, InTerm ]

        Value ->
            [ WordTerm, MultiQuotedWord, InValue ]

        InValue ->
            [ SpaceTerm, OpenParenthesisTerm, InRepeatValue, CloseParenthesisTerm ]

        InRepeatValue ->
            [ SpaceTerm, CommaTerm, Value, InRepeatValue ]

        MultiQuotedWord ->
            [ StartQuoteTerm, Statement, EndQuoteTerm ]

        EitherOrOperator ->
            [ SpaceTerm, EitherTerm, SpaceTerm, Value, SpaceTerm, OrTerm, SpaceTerm, EitherOrOperator ]

        NeitherNorOperator ->
            [ SpaceTerm, NeitherTerm, SpaceTerm, Value, SpaceTerm, NorTerm, SpaceTerm, NeitherNorOperator ]

        Conjunction ->
            [ OperatorGroupOr, OperatorGroupAnd ]

        OperatorGroupOr ->
            [ SpaceTerm, OrTerm ]

        OperatorGroupAnd ->
            [ SpaceTerm, AndTerm ]

        _ ->
            []
