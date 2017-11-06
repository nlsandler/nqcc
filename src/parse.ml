(* GRAMMAR:
 * program -> function-definition
 * function-definition -> type id ( argument_list ) fun_body
 * argument_list -> type id | type id argument_list
 * fun_body -> { statement_list }
 * statement_list -> statement | statement statement_list
 * statement -> return | return exp
 * exp -> int
 *)
let rec parse_fun_params = function
    | Tok.CloseParen::rest -> ([], rest)
    | Tok.IntKeyword::(Tok.Id name)::rest -> 
        let other_params, rest = parse_fun_params rest in
        (Ast.Param(Ast.IntType, Ast.ID(name))::other_params, rest)
    | Tok.CharKeyword::(Tok.Id name)::rest -> 
        let other_params, rest = parse_fun_params rest in
        (Ast.Param(Ast.CharType, Ast.ID(name))::other_params, rest)
    | _ -> failwith("Parse error in parse_fun_params")

let tok_to_const = function
    | Tok.Int i -> Ast.Const(Ast.Int i)
    | Tok.Char c -> Ast.Const(Ast.Char c)
    | _ -> failwith("Not a constant") 

let rec parse_factor toks =
    match toks with
    | Tok.OpenParen::factor -> 
        let exp, after_exp = parse_exp factor in
        (match after_exp with
        | (Tok.CloseParen)::rest -> (exp, rest)
        | _ -> failwith("Syntax error: expected close paren"))
    | Tok.Minus::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Negate, num), rest
    | Tok.Plus::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Pos, num), rest
    | Tok.Complement::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Complement, num), rest
    | Tok.Int(i)::rest -> Ast.Const(Ast.Int(i)), rest
    | Tok.Char(c)::rest -> Ast.Const(Ast.Char(c)), rest
    | _ -> failwith("Failed to parse factor")

and build_term left_factor toks =
    match toks with
    | (Tok.Mult)::right ->
        let right_factor, rest = parse_factor right in
        let left_factor = (Ast.BinOp(Ast.Mult, left_factor, right_factor)) in
        build_term left_factor rest
    | (Tok.Div)::right ->
        let right_factor, rest = parse_factor right in
        let left_factor = (Ast.BinOp(Ast.Div, left_factor, right_factor)) in
        build_term left_factor rest    
    | _ -> left_factor, toks

and parse_term toks =
    let left, rest = parse_factor toks in
    build_term left rest

and build_exp left_term toks =
    match toks with
    | (Tok.Plus)::right -> 
        let right_term, rest = parse_term right in
        let left_term = (Ast.BinOp(Ast.Add, left_term, right_term)) in
        build_exp left_term rest
    | (Tok.Minus)::right ->
        let right_term, rest = parse_term right in
        let left_term = (Ast.BinOp(Ast.Sub, left_term, right_term)) in
        build_exp left_term rest        
    | _ -> left_term, toks 

and parse_exp toks =
    let left, rest = parse_term toks in
    build_exp left rest

let rec parse_statement_list tokens =
    match tokens with
    | Tok.ReturnKeyword::Tok.Semicolon::rest -> [Ast.Return], rest
    | Tok.ReturnKeyword::rest ->
        let exp, rest = parse_exp rest in
            if (List.hd rest == Tok.Semicolon)
            then [Ast.ReturnVal(exp)], List.tl rest
            else failwith("Expected semicolon at end of statement")
    | _ -> failwith("Invalid statement")

let parse_fun_body tokens = (* Assume for now there's nothing after function body *)
    let statements, rest = parse_statement_list tokens in
    match rest with
    | Tok.CloseBrace::[] -> Ast.Body(statements)
    | _ -> failwith("Expected closing brace")

let parse_fun tokens =
    let fun_type, fun_name, rest = 
        match tokens with
        | Tok.IntKeyword::(Tok.Id name)::Tok.OpenParen::rest -> (Ast.IntType, Ast.ID(name), rest)
        | Tok.CharKeyword::(Tok.Id name)::Tok.OpenParen::rest -> (Ast.CharType, Ast.ID(name), rest)
        | _ -> failwith("Parse error in parse_fun: bad function type or name") in
    let fun_params, rest = parse_fun_params rest in
    let fun_body = 
        match rest with
        | Tok.OpenBrace::rest -> parse_fun_body rest
        | _ -> failwith("Expected brace to open function body") in
    Ast.FunDecl(fun_type, fun_name, fun_params, fun_body)

let parse tokens = Ast.Prog(parse_fun tokens)