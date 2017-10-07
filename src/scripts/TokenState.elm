module TokenState exposing (..)


type TokenState
    = Start
    | Statement
    | SpaceTerm
    | WordTerm
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
    | EitherOrTerm
    | AndTerm
    | OrTerm
    | NorTerm
    | NotTerm
    | IsTerm
    | IsOperator
    | IsSubOperator
    | EitherTerm
    | EitherOrOperator
    | NeitherTerm
    | NeitherNorOperator
    | UnknownKeywordTerm
    | InTerm
    | InOperator
    | InValue
    | OpenParenthesisTerm
    | CloseParenthesisTerm
    | CommaTerm
    | InRepeatValue
