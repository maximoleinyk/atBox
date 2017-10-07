module Parser exposing (ASTNode, run)

import Lexer exposing (Lexeme)
import Model exposing (Model)


type alias ASTNode =
    {}


run : List Lexeme -> Model -> ASTNode
run lexemes model =
    {}
