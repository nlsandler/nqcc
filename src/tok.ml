(* A token in a C progrm *)
type token = 
    | OpenBrace
    | CloseBrace
    | OpenParen
    | CloseParen
    | Semicolon
    | IntKeyword
    | CharKeyword
    | ReturnKeyword
    | Plus
    | Int of int
    | Char of char
    | Id of string