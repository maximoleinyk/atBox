module FsmState exposing (..)


type FsmState
    = Start
    | Statement
    | Space
    | Word
    | SpaceTerm
    | CharTerm
