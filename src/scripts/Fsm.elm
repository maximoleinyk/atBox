module Fsm exposing (getPossibleStates)

import FsmState exposing (FsmState(..))


getPossibleStates : FsmState -> List FsmState
getPossibleStates state =
    case state of
        Start ->
            [ Statement, Criteria ]

        Statement ->
            [ Space, Word, Statement ]

        Space ->
            [ SpaceTerm, Space ]

        SpaceTerm ->
            []

        Word ->
            [ WordTerm, Word ]

        WordTerm ->
            []

        Criteria ->
            [ Criterion, Criteria ]

        Criterion ->
            [ OperatorGroup, Conjunction ]

        OperatorGroup ->
            [ Keyword, Space, Operator ]

        Keyword ->
            [ Space, KeywordTerm ]

        KeywordTerm ->
            []

        Operator ->
            [ IsOperator ]

        IsOperator ->
            [ Space, IsTerm, IsSubOperator, Value ]

        IsTerm ->
            []

        IsSubOperator ->
            [ NotOperator, EitherOperator, NeitherOperator ]

        NotOperator ->
            [ Space, NotTerm, Value ]

        NotTerm ->
            []

        Value ->
            [ Space, Word, MultiQuotedWord ]

        MultiQuotedWord ->
            [ StartQuoteTerm, Statement, EndQuoteTerm ]

        StartQuoteTerm ->
            []

        EndQuoteTerm ->
            []

        EitherOperator ->
            [ Space, EitherTerm, Value, EitherOrOperator ]

        EitherTerm ->
            []

        EitherOrOperator ->
            [ Space, OrTerm, Value, EitherOrOperator ]

        OrTerm ->
            []

        NeitherOperator ->
            [ Space, NeitherTerm, Value, NeitherNorOperator ]

        NeitherTerm ->
            []

        NeitherNorOperator ->
            [ Space, NorTerm, Value, NeitherNorOperator ]

        NorTerm ->
            []

        Conjunction ->
            [ OperatorGroupOr, OperatorGroupAnd ]

        OperatorGroupOr ->
            [ Space, OrTerm, Space, Criterion ]

        OperatorGroupAnd ->
            [ Space, AndTerm, Space, Criterion ]

        AndTerm ->
            []
