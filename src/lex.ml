open Batteries

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
        (*
            int_regexp: (-?[0-9]+)(\b.* ) 
        *) 
        let int_regexp = Str.regexp "\\(-?[0-9]+\\)\\(\\b.*\\)" (* TODO: handle more int representations (e.g. hex) *)
        (*
            id_regexp: ([A-Za-z][A-Za-z0-9_]* )(\b.* ) 
        *)
        let id_regexp = Str.regexp "\\([A-Za-z][A-Za-z0-9_]*\\)\\(\\b.*\\)"
        (*
            char_regexp: ('[^\\]|\\([abfenrtv'?]|[0-7]{1,3}|[0-9a-f]{1,3})')(.* )
            in other words:
            -a single quote
            -one character that isn't a backslash or single quote, OR
            -a backslash followed by:
                -a, b, f, e, n, r, t, v, backslash, question mark, single quote or double quote (escape sequences), OR
                -1, 2, or 3 digits between 0 and 7 (octal representation)
                -h, followed by 1, 2, or 3 digits between 0-9, a-f (hex representation)
            -a single closing quote
            -everything else
        *)
        let char_regexp = Str.regexp "'\\([^'\\\\]\\|\\\\\\([abfenrtv'\"?]\\|[0-7]+\\|x[0-9a-f]+\\)\\)'\\(.*\\)"

        let get_char char_token = 
            if String.length char_token = 1 then String.get char_token 0 else
            if String.length char_token = 2 then (* escape sequence *)
                match char_token with
                | "\\" -> Char.chr 92   (* backslash *)
                | "\\a" -> Char.chr 7   (* bell *)
                | "\\b" -> '\b'         (* backspace *)
                | "\\e" -> Char.chr 27  (* esc *)
                | "\\f" -> Char.chr 12  (* form feed *)
                | "\\n" -> '\n'         (* newline *)
                | "\\r" -> '\r'         (* carriage return *)
                | "\\t" -> '\t'         (* tab *)
                | "\\v" -> Char.chr 11  (* vertical tab *)
                | "\\\'" -> Char.chr 39 (* single quote *)
                | "\\\"" -> '"'         (* double quote *)
                | "\\?" -> '?'          (* question mark *)
                | _ -> failwith("Unknown escape sequence "^char_token) else
            if String.get char_token 1 = 'x' then (* hex string *)
                let num_str = "0"^(String.slice ~first:1 char_token) in
                Char.chr (Int.of_string num_str) 
            else 
                let num_str = "0o"^(String.slice ~first:1 char_token) in
                Char.chr (Int.of_string num_str)

        let get_const_or_id input =
            if Str.string_match int_regexp input 0
            then (* it's an int *)
                let int_token = Str.matched_group 1 input in
                let rest = Str.matched_group 2 input in
                    (Int(int_of_string int_token), rest)
            else if Str.string_match char_regexp input 0
            then (* it's a char *)
                let char_token = Str.matched_group 1 input in
                let rest = Str.matched_group 3 input in
                let c = get_char char_token in 
                (Char(c), rest)
            else if Str.string_match id_regexp input 0
            then (* it's an ID, possibly a keyword *)
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
                        match String.explode input with
                        | '{'::rest -> (OpenBrace, String.implode rest)
                        | '}'::rest -> (CloseBrace, String.implode rest)
                        | '('::rest -> (OpenParen, String.implode rest)
                        | ')'::rest -> (CloseParen, String.implode rest)
                        | ';'::rest -> (Semicolon, String.implode rest)
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