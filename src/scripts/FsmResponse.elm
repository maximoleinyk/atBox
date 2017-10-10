module FsmResponse exposing (..)

import Lexer exposing (Lexeme)
import Parser exposing (AST)
import Tokenizer exposing (Token)


type alias FsmResponse =
    { tokens : List Token
    , lexemes : List Lexeme
    , ast : AST
    }
