(* Convert C program into a list of tokens *)
val lex : string -> Tok.token list

(* Get string representation of a token *)
val tok_to_string: Tok.token -> string
