module GlobalTypes
    exposing
        ( AST(..)
        , Config
        , CursorContext(..)
        , FsmResponse
        , Lexeme
        , LexemeType(..)
        , LexerState(..)
        , Model
        , Msg(..)
        , OperatorType(..)
        , QueryField
        , Token
        , TokenState(..)
        , TranslatorOutput(..)
        , ValueType(..)
        )

import Dict exposing (Dict)
import Dom


type TranslatorOutput
    = AndOutput { and : List TranslatorOutput }
    | OrOutput { or : List TranslatorOutput }
    | EndOutput
        { field : String
        , operator : String
        , value : String
        }
    | NoOutput


type OperatorType
    = IsType
    | IsNotType
    | IsEitherType
    | IsNeitherType
    | IsInType
    | IsNotInType


type ValueType
    = ValueStringType
    | ValueDateType
    | ValueNumberType


type CursorContext
    = NoContext
    | KeywordContext (Dict String String) String
    | OperatorContext (Dict String String) String
    | ValueContext (Dict String String) String
    | FreeContext (Dict String String) String
    | ValueSeparatorContext ValueType (Dict String String) String
    | JoinerContext (Dict String String) String


type Msg
    = UpdateValue String
    | SelectHighlightedValue
        { itemKey : String
        , replacementValue : String
        }
    | EnterKeyPressed
    | ArrowUpPressed
    | ArrowDownPressed
    | TabKeyPressed
    | GetCursorPosition
    | Focus
    | Blur
    | Parse
    | UpdateCursorPosition Int
    | UpdateCursorPositionFailed
    | ArrowLeftPressed
    | ArrowRightPressed
    | FocusResult (Result Dom.Error ())


type AST
    = Node
        { left : AST
        , value : String
        , right : AST
        }
    | Leaf String
    | Null


type TokenState
    = Start
    | Statement
    | TokenValue
    | MultiQuotedWord
    | SingleWord
    | Criteria
    | Criterion
    | Conjunction
    | OrConjunction
    | AndConjunction
    | TokenOperator
    | OperatorGroup
    | IsOperator
    | IsEitherOperator
    | IsNeitherOperator
    | IsNotOperator
    | IsInOperator
    | IsNotInOperator
    | InValue
    | SpaceTerm
    | WordTerm
    | KeywordTerm
    | StartQuoteTerm
    | EndQuoteTerm
    | EitherOrTerm
    | AndTerm
    | OrTerm
    | NeitherNorTerm
    | IsNotTerm
    | IsTerm
    | IsEitherTerm
    | IsNeitherTerm
    | UnknownKeywordTerm
    | IsInTerm
    | IsNotInTerm
    | OpenParenthesisInOperatorTerm
    | CloseParenthesisInOperatorTerm
    | CommaTerm
    | OpenParenthesisTerm
    | CloseParenthesisTerm


type LexerState
    = START
    | JOIN_TERM
    | EXPRESSION
    | OPEN_PARENTHESIS_TERM
    | CLOSE_PARENTHESIS_TERM
    | OPERATOR_GROUP
    | FIELD_TERM
    | OPERATOR_TERM
    | VALUE_TERM
    | OPEN_PARENTHESIS
    | CLOSE_PARENTHESIS


type LexemeType
    = Field
    | Operator OperatorType
    | LexemeValue
    | Joiner
    | LeftParenthesis
    | RightParenthesis


type alias QueryField =
    { field : String
    , label : String
    , fieldType : String
    , values : List String
    }


type alias Token =
    { state : TokenState
    , value : String
    , index : Int
    }


type alias Lexeme =
    { lexemeType : LexemeType
    , value : String
    , index : Int
    }


type alias Model =
    { id : String
    , label : String
    , placeholder : String
    , value : String
    , queryFields : List QueryField
    , context : CursorContext
    , selectedItem : String
    , keywordDelimiter : String
    , cursorIndex : Int
    , focused : Bool
    }


type alias FsmResponse =
    { tokens : List Token
    , lexemes : List Lexeme
    , ast : AST
    , output : TranslatorOutput
    , string : String
    }


type alias Config =
    { id : String
    , label : String
    , placeholder : String
    , queryFields : List QueryField
    , value : String
    }
