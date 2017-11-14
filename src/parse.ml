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

let rec parse_function_call = function
    | Tok.Id(name)::Tok.OpenParen::arg_tokens ->
        let fun_name = Ast.ID(name) in
        let args, rest = parse_function_arguments arg_tokens in
        Ast.FunCall(fun_name, args), rest
    | _ -> failwith("Shouldn't have called parse_function_call, this isn't a function call")

and parse_function_arguments = function
    | Tok.CloseParen::rest -> [], rest
    | toks ->
        let arg, rest = parse_exp toks in
        let args, rest = 
            match rest with
            | Tok.Comma::more_args -> parse_function_arguments more_args
            | Tok.CloseParen::after_fun_call -> [], after_fun_call
            | _ -> failwith("Invalid list of function arguments")  in
        arg::args, rest

and parse_factor toks =
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
    | Tok.Bang::factor ->
        let num, rest = parse_factor factor in
        Ast.UnOp(Ast.Not, num), rest
    | Tok.Id(name)::Tok.OpenParen::rest -> parse_function_call toks
    | Tok.Id(name)::rest -> Ast.Var(Ast.ID(name)), rest
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


let parse_declaration var_type tokens =
    match tokens with
    | Tok.Id(varname)::rest ->
        let var_id = Ast.ID(varname) in
        let init, rest =
            match rest with
            | Tok.Semicolon::rest_of_statements -> None, rest
            | Tok.Eq::rest -> 
                let exp, rest = parse_exp rest in
                Some(exp), rest
            | _ -> failwith("Invalid initial value for variable")
        in Ast.DeclareVar(var_type, var_id, init), rest
    | _ -> failwith("Invalid variable declaration")

let parse_assignment = function
    | Tok.Id(varname)::Tok.Eq::rest ->
        let exp, rest = parse_exp rest in
        Ast.Assign(Ast.ID(varname), exp), rest
    | _ -> failwith("Invalid assignment statement")

let parse_return_statement = function
    | Tok.ReturnKeyword::(Tok.Semicolon::_ as rest) -> Ast.Return, rest
    | Tok.ReturnKeyword::rest ->
        let exp, rest = parse_exp rest in
        Ast.ReturnVal(exp), rest
    | _ -> failwith("Expected return statement")

let rec parse_if_statement = function
    | Tok.IfKeyword::(Tok.OpenParen::_ as toks) ->
        let cond, rest = parse_exp toks in
        let if_body, rest = parse_statement_or_block rest in
        let else_body, rest =
            match rest with
            | Tok.ElseKeyword::else_tokens ->
                let else_body, rest = parse_statement_or_block else_tokens in
                Some(else_body), rest
            | _ -> None, rest in
        let if_statement = Ast.If(cond, if_body, else_body) in
        if_statement, rest
    | Tok.IfKeyword::_ -> failwith("Expected '(' after 'if'")
    | _ -> failwith("PANIC: parse_if_statement called on non-if statement")

and parse_statement_or_block tokens =
    if (List.hd tokens) == Tok.OpenBrace
    then parse_statement_block tokens
    else
        let stmt, rest = parse_statement tokens in
        [stmt], rest

and parse_statement_block = function
    | Tok.OpenBrace::statements ->
        let statement_list, after_list = parse_statement_list statements in
        (match after_list with
        | Tok.CloseBrace::rest -> statement_list, rest
        | _ -> failwith("Expected '}' after statement list"))
    | _ -> failwith("PANIC: parse_statement_block called on non statement block")

(* TODO: actually pay attention to types 
 * also allow plain expressions that aren't assignments
 *)
and parse_statement tokens =
    let stmt, rest =
        match tokens with
        | Tok.IntKeyword::rest -> parse_declaration Ast.IntType rest
        | Tok.CharKeyword::rest -> parse_declaration Ast.CharType rest
        | Tok.IfKeyword::rest -> parse_if_statement tokens
        | Tok.Id(v)::Tok.Eq::rest -> parse_assignment tokens
        | Tok.ReturnKeyword::rest -> parse_return_statement tokens
        | _ -> failwith("Invalid statement") in
    match stmt, rest with
    | Ast.If(_), _ -> stmt, rest (* no semicolon after if *)
    | _, Tok.Semicolon::rest -> stmt, rest (* eat semicolon from end of statement *)
    | _ -> failwith("Expected semicolon at end of statement") 

and parse_statement_list tokens =
    if (List.hd tokens) == Tok.CloseBrace
    then [], tokens
    else
        let next_statement, rest = parse_statement tokens in
        let statements, rest = parse_statement_list rest in
        next_statement::statements, rest
(*
    match tokens with
    | Tok.ReturnKeyword::rest -> 
        let ret, rest = parse_return_statement tokens in
        [ret], rest 
    | _ ->
        let next_statement, rest = parse_statement tokens in
            if (List.hd rest == Tok.Semicolon)
            then 
                let statements, rest = parse_statement_list (List.tl rest) in
                next_statement::statements, rest
            else failwith("Expected semicolon at end of statement")
*)        

let parse_fun_body tokens = (* Assume for now there's nothing after function body *)
    let statements, rest = parse_statement_list tokens in
    match rest with
    | Tok.CloseBrace::rest -> Ast.Body(statements), rest
    | _ -> failwith("Expected closing brace")

let parse_fun tokens =
    let fun_type, fun_name, rest = 
        match tokens with
        | Tok.IntKeyword::(Tok.Id name)::Tok.OpenParen::rest -> (Ast.IntType, Ast.ID(name), rest)
        | Tok.CharKeyword::(Tok.Id name)::Tok.OpenParen::rest -> (Ast.CharType, Ast.ID(name), rest)
        | _ -> failwith("Parse error in parse_fun: bad function type or name") in
    let fun_params, rest = parse_fun_params rest in
    let fun_body, rest = 
        match rest with
        | Tok.OpenBrace::rest -> parse_fun_body rest
        | _ -> failwith("Expected brace to open function body") in
    Ast.FunDecl(fun_type, fun_name, fun_params, fun_body), rest

let rec parse_funs = function
    | [] -> [] (* no functions left to parse *)
    | tokens ->
        let f, rest = parse_fun tokens in
        let fs = parse_funs rest in
        f::fs

let parse tokens = Ast.Prog(parse_funs tokens)