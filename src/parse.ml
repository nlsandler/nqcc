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

let rec parse_exp toks =
    match toks with
    | (Tok.Int i)::(Tok.Plus)::rest -> 
        let e1 = Ast.Const(Ast.Int i) in
        let e2, rest = parse_exp rest in
        (Ast.BinOp(Ast.Add, e1, e2), rest)
    | (Tok.Char c)::(Tok.Plus)::rest -> 
        let e1 = Ast.Const(Ast.Char c) in
        let e2, rest = parse_exp rest in
        (Ast.BinOp(Ast.Add, e1, e2), rest)
    | Tok.OpenParen::rest ->
        let e1, rest = parse_exp rest  in
        (match rest with
        | Tok.CloseParen::Tok.Plus::rest_of_exp ->
            let e2, rest = parse_exp rest_of_exp in
            (Ast.BinOp(Ast.Add, e1, e2), rest)
        | Tok.CloseParen::rest_after_exp -> (e1, rest_after_exp)
        | _ -> failwith("Missing ')'") )
    | (Tok.Int i)::rest -> (Ast.Const(Ast.Int i), rest)
    | (Tok.Char c)::rest -> (Ast.Const(Ast.Char c), rest)
    | _ -> failwith("Invalid expression")

(*
    | (Tok.Int i)::rest -> (Ast.Const(Ast.Int(i)), rest)
    | (Tok.Char c)::rest -> (Ast.Const(Ast.Char(c)), rest)
    | tok::rest -> failwith("Unrecognized token "^(Lex.tok_to_string tok)^" in parse_exp")
    | [] -> failwith("Expected expression in parse_exp but none found")
*)
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