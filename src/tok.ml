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
    | Complement
    | Plus
    | Minus
    | Mult
    | Div
    | Int of int
    | Char of char
    | Id of string