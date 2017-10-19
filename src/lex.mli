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
    | Int of int
    | Char of char
    | Id of string

(* Convert C program into a list of tokens *)
val lex : string -> token list

(* Get string representation of a token *)
val tok_to_string: token -> string