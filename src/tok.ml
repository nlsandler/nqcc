(* A token in a C progrm *)
type token = 
    | OpenBrace
    | CloseBrace
    | OpenParen
    | CloseParen
    | Comma
    | Semicolon
    | IntKeyword
    | CharKeyword
    | ReturnKeyword
    | IfKeyword
    | ElseKeyword
    | Bang
    | Complement
    | Plus
    | Minus
    | Mult
    | Div
    | Eq (* = *)
    | DoubleEq (* == *)
    | Neq (* != *)
    | Lt (* < *)
    | Le (* <= *)
    | Gt (* > *)
    | Ge (* >= *)
    | Int of int
    | Char of char
    | Id of string