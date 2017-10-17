module GlobalTypes
    exposing
        ( AST(..)
        , Config
        , CursorContext(..)
        , FsmResponse
        , Lexeme
        , LexemeState(..)
        , LexerState(..)
        , Model
        , Msg(..)
        , OperatorType(..)
        , OutputOperatorType(..)
        , OutputValueType(..)
        , QueryField
        , Term(..)
        , Token
        , TokenState(..)
        , TranslatorOutput(..)
        , TranslatorOutputValueType(..)
        , ValueType(..)
        )

import Dict exposing (Dict)
import Dom


type TranslatorOutputValueType
    = Single String
    | Multiple (List String)
    | None


type Term
    = Expression (List Term)
    | Operand Lexeme
    | AndOperator
    | OrOperator
    | OpenParenthesis
    | CloseParenthesis


type TranslatorOutput
    = AndOutput
        { and : List TranslatorOutput
        }
    | OrOutput
        { or : List TranslatorOutput
        }
    | EndOutput
        { field : String
        , operator : String
        , value : TranslatorOutputValueType
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
    | Init
    | Focus
    | Blur
    | Process
    | UpdateCursorPosition Int
    | UpdateCursorPositionFailed
    | ArrowLeftPressed
    | ArrowRightPressed
    | FocusResult (Result Dom.Error ())


type OutputOperatorType
    = IsOperatorType
    | IsNotOperatorType
    | IsEitherOperatorType
    | IsNeitherOperatorType
    | IsInOperatorType
    | IsNotInOperatorType
    | OrOperatorType
    | AndOperatorType
    | NoOutputType


type OutputValueType
    = SingleValue String
    | MultipleValues (List String)
    | NoValue


type AST
    = Node
        { left : AST
        , value : OutputOperatorType
        , right : AST
        }
    | Leaf OutputValueType
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


type LexemeState
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
    { state : LexemeState
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
    }


type alias Config =
    { id : String
    , label : String
    , placeholder : String
    , queryFields : List QueryField
    , value : String
    }
