module Lex : sig
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
    (* type token *)
    val lex : string -> token list
    val tok_to_string: token -> string
end =
    struct
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
        let int_regexp = Str.regexp "\\(-?[0-9]+\\)\\(\\b.*\\)" (* TODO: handle more int representations (e.g. hex) *)
        let id_regexp = Str.regexp "\\([A-Za-z][A-Za-z0-9_]*\\)\\(\\b.*\\)" 
        let char_regexp = Str.regexp "'[^\\\\]'\\(.*\\)" (* TODO: handle escape sequences *)
        let get_const_or_id input =
            if Str.string_match int_regexp input 0
            then 
                let int_token = Str.matched_group 1 input in
                let rest = Str.matched_group 2 input in
                    (Int(int_of_string int_token), rest)
            else if Str.string_match char_regexp input 0
            then
                let rest = Str.matched_group 1 input in
                    (Char(String.get input 1), rest)
            else if Str.string_match id_regexp input 0
            then
                let id_token_str = Str.matched_group 1 input in
                let rest = Str.matched_group 2 input in
                let id_token = 
                    match id_token_str with
                    | "return" -> ReturnKeyword
                    | "int" -> IntKeyword
                    | "char" -> CharKeyword
                    | _ -> Id(id_token_str) in
                    (id_token, rest)
            else
                failwith ("Syntax error: \""^input^ "\" is not valid.")

        let rec lex input = 
            let input = String.trim input in 
                if String.length input = 0
                then []
                else
                    let tok, remaining_program = 
                        match Util.explode input with
                        | '{'::rest -> (OpenBrace, Util.implode rest)
                        | '}'::rest -> (CloseBrace, Util.implode rest)
                        | '('::rest -> (OpenParen, Util.implode rest)
                        | ')'::rest -> (CloseParen, Util.implode rest)
                        | ';'::rest -> (Semicolon, Util.implode rest)
                        | _ -> get_const_or_id input in
                    tok :: (lex remaining_program)

        let tok_to_string t =
            match t with
            | OpenBrace -> "{"
            | CloseBrace -> "}"
            | OpenParen -> "("
            | CloseParen -> ")"
            | Semicolon -> ";"
            | IntKeyword -> "INT"
            | CharKeyword -> "CHAR"
            | ReturnKeyword -> "RETURN"
            | Int i -> Printf.sprintf "INT<%d>" i
            | Id id -> Printf.sprintf "ID<%s>" id
            | Char c -> Printf.sprintf "CHAR<%c>" c
    end