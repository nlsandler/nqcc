open Batteries

(*
    int_regexp: (-?([0-9]+)|(0x[0-9a-f]+))(\b.* )
    in other words:
    -one or more digits, OR
    -0x followed by one or more hex digits (0-9, a-f)
    -everything else
    all case insensitive
*) 
let int_regexp = Str.regexp_case_fold "\\(\\([0-9]+\\)\\|\\(0x[0-9a-f]+\\)\\)\\(\\b.*\\)"
(*
    id_regexp: ([A-Za-z][A-Za-z0-9_]* )(\b.* ) 
*)
let id_regexp = Str.regexp "\\([A-Za-z_][A-Za-z0-9_]*\\)\\(\\b.*\\)"
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
let char_regexp = Str.regexp "'\\([^'\\\\]\\|\\\\\\([abfenrtv'\"?\\\\]\\|[0-7]+\\|x[0-9a-fA-F]+\\)\\)'\\(.*\\)"

let get_char char_token = 
    match String.length char_token with
    | 1 -> String.get char_token 0 (* a single character *)
    | 2 -> (* escape sequence *)
        begin match char_token with
        | "\\\\" -> Char.chr 92   (* backslash *)
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
        | _ -> failwith("Unknown escape sequence "^char_token)
        end
    | _ -> (* different prefix for hex or octal *)
        let prefix = if String.get char_token 1 = 'x' then "0" else "0o" in 
        let num_str = prefix^(String.slice ~first:1 char_token) in
        Char.chr (Int.of_string num_str)

let get_int int_token =
    if (String.get int_token 0 = '0') &&
        String.length int_token > 1 &&
        (Char.lowercase (String.get int_token 1) <> 'x') 
    then int_of_string ("0o"^int_token) (* octal *)
    else int_of_string int_token (* hex or decimal *)

let get_id t =
    let open Tok in
    match t with
    | "return" -> ReturnKeyword
    | "int" -> IntKeyword
    | "char" -> CharKeyword
    | "if" -> IfKeyword
    | "else" -> ElseKeyword
    | "for" -> ForKeyword
    | "do" -> DoKeyword
    | "while" -> WhileKeyword
    | _ -> Id t

let lex_complex_token input =
    if Str.string_match int_regexp input 0
    then (* it's an int *) 
        let int_token = Str.matched_group 1 input in
        let int_val = get_int int_token in
        if int_val > Int32.to_int Int32.max_int || int_val < Int32.to_int Int32.min_int
        then failwith("Invalid int literal") 
        else
            let rest = Str.matched_group 4 input in
            (Tok.Int int_val), rest
    else if Str.string_match char_regexp input 0
    then (* it's a char *)
        let char_token = Str.matched_group 1 input in
        let rest = Str.matched_group 3 input in
        (Tok.Char (get_char char_token)), rest
    else if Str.string_match id_regexp input 0
    then (* it's an ID, possibly a keyword *)
        let id_token_str = Str.matched_group 1 input in
        let rest = Str.matched_group 2 input in
        let id_token = get_id id_token_str in
        id_token, rest
    else
        failwith ("Syntax error: \""^input^ "\" is not valid.")

let rec lex_const_or_id input_toks =
    let input = String.trim (String.implode input_toks) in
    let tok, rest = lex_complex_token input in
    tok::(lex_rest (String.explode rest))

and lex_rest words = 
    let open Tok in
    match words with
    | [] -> []
    | '{'::rest -> OpenBrace::(lex_rest rest)
    | '}'::rest -> CloseBrace::(lex_rest rest)
    | '('::rest -> OpenParen::(lex_rest rest)
    | ')'::rest -> CloseParen::(lex_rest rest)
    | ';'::rest -> Semicolon::(lex_rest rest)
    | ','::rest -> Comma::(lex_rest rest)
    | '+'::rest -> Plus::(lex_rest rest)
    | '?'::rest -> Question::(lex_rest rest)
    | ':'::rest -> Colon::(lex_rest rest)
    | '-'::'-'::rest -> failwith("decrement not yet implemented")
    | '<'::'<'::rest -> ShiftLeft::(lex_rest rest)
    | '>'::'>'::rest -> ShiftRight::(lex_rest rest)
    | '!'::'='::rest -> Neq::(lex_rest rest)
    | '<'::'='::rest -> Le::(lex_rest rest)
    | '>'::'='::rest -> Ge::(lex_rest rest)
    | '<'::rest -> Lt::(lex_rest rest)
    | '>'::rest -> Gt::(lex_rest rest)
    | '-'::rest -> Minus::(lex_rest rest)
    | '*'::rest -> Mult::(lex_rest rest)
    | '/'::rest -> Div::(lex_rest rest)
    | '%'::rest -> Mod::(lex_rest rest)
    | '&'::'&'::rest -> And::(lex_rest rest)
    | '&'::rest -> BitAnd::(lex_rest rest)
    | '|'::'|'::rest -> Or::(lex_rest rest)
    | '|'::rest -> BitOr::(lex_rest rest)
    | '^'::rest -> Xor::(lex_rest rest)
    | '~'::rest -> Complement::(lex_rest rest)
    | '!'::rest -> Bang::(lex_rest rest)
    | '='::'='::rest -> DoubleEq::(lex_rest rest)
    | '='::rest -> Eq::(lex_rest rest)
    | c::rest -> 
        if Char.is_whitespace c then lex_rest rest else lex_const_or_id words

let lex input =
    let input = String.trim input in
    lex_rest (String.explode input)

let tok_to_string t =
    let open Tok in
    match t with
    | OpenBrace -> "{"
    | CloseBrace -> "}"
    | OpenParen -> "("
    | CloseParen -> ")"
    | Semicolon -> ";"
    | Colon -> ":"
    | Question -> "?"
    | Comma -> ","
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Complement -> "~"
    | Bang -> "!"
    | Eq -> "="
    | DoubleEq -> "=="
    | Neq -> "!="
    | Le -> "<="
    | Ge -> ">="
    | Lt -> "<"
    | Gt -> ">"
    | And -> "&&"
    | Or -> "||"
    | BitAnd -> "&"
    | BitOr -> "|"
    | Xor -> "^"
    | ShiftLeft -> "<<"
    | ShiftRight -> ">>"
    | IntKeyword -> "INT"
    | CharKeyword -> "CHAR"
    | ReturnKeyword -> "RETURN"
    | IfKeyword -> "IF"
    | ElseKeyword -> "ELSE"
    | ForKeyword -> "FOR"
    | DoKeyword -> "DO"
    | WhileKeyword -> "WHILE"
    | Int i -> Printf.sprintf "INT<%d>" i
    | Id id -> Printf.sprintf "ID<%s>" id
    | Char c -> Printf.sprintf "CHAR<%c" c