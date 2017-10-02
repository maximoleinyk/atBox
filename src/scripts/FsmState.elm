module FsmState exposing (..)


type FsmState
    = Start
    | Statement
    | Space
    | SpaceTerm
    | Word
    | WordTerm
    | Keyword
    | KeywordTerm
    | Value
    | MultiQuotedWord
    | StartQuoteTerm
    | EndQuoteTerm
    | Criteria
    | Criterion
    | Conjunction
    | Operator
    | OperatorGroup
    | OperatorGroupOr
    | OperatorGroupAnd
    | AndTerm
    | OrTerm
    | NorTerm
    | NotTerm
    | IsTerm
    | IsOperator
    | IsSubOperator
    | NotOperator
    | EitherTerm
    | EitherOperator
    | EitherOrOperator
    | NeitherTerm
    | NeitherOperator
    | NeitherNorOperator
