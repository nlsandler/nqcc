(* GRAMMAR:
 * program -> function-definition
 * function-definition -> type id ( argument_list ) fun_body
 * argument_list -> type id | type id argument_list
 * fun_body -> { statement_list }
 * statement_list -> statement | statement statement_list
 * statement -> return | return exp
 * exp -> int
 *)
 open Lex
 open Ast

 module Parse : sig
    val parse : Lex.token list -> AST.prog
end =
struct
    let rec parse_fun_params = function
        | Lex.CloseParen::rest -> ([], rest)
        | Lex.IntKeyword::(Lex.Id name)::rest -> 
            let other_params, rest = parse_fun_params rest in
            (AST.Param(AST.IntType, AST.ID(name))::other_params, rest)
        | Lex.CharKeyword::(Lex.Id name)::rest -> 
            let other_params, rest = parse_fun_params rest in
            (AST.Param(AST.CharType, AST.ID(name))::other_params, rest)
        | _ -> failwith("Parse error in parse_fun_params")

    let parse_exp = function
        | (Lex.Int i)::rest -> (AST.Const(AST.Int(i)), rest)
        | tok::rest -> failwith("Unrecognized token "^(Lex.tok_to_string tok)^" in parse_exp")
        | [] -> failwith("Expected expression in parse_exp but none found")

    let rec parse_statement_list tokens =
        match tokens with
        | Lex.Semicolon::rest -> parse_statement_list rest
        | Lex.ReturnKeyword::Lex.Semicolon::rest -> 
            let other_statements, rest = parse_statement_list rest in
            AST.Return::other_statements, rest
        | Lex.ReturnKeyword::rest ->
            let exp, rest = parse_exp rest in
            let other_statements, rest = parse_statement_list rest in
            AST.ReturnVal(exp)::other_statements, rest
        | _ -> ([], tokens) (* Whatever is left isn't part of statement list *)

    let parse_fun_body tokens = (* Assume for now there's nothing after function body *)
        let statements, rest = parse_statement_list tokens in
        match rest with
        | Lex.CloseBrace::[] -> AST.Body(statements)
        | _ -> failwith("Expected closing brace")

    let parse_fun tokens =
        let fun_type, fun_name, rest = 
            match tokens with
            | Lex.IntKeyword::(Lex.Id name)::Lex.OpenParen::rest -> (AST.IntType, AST.ID(name), rest)
            | Lex.CharKeyword::(Lex.Id name)::Lex.OpenParen::rest -> (AST.CharType, AST.ID(name), rest)
            | _ -> failwith("Parse error in parse_fun: bad function type or name") in
        let fun_params, rest = parse_fun_params rest in
        let fun_body = 
            match rest with
            | Lex.OpenBrace::rest -> parse_fun_body rest
            | _ -> failwith("Expected brace to open function body") in
        AST.FunDecl(fun_type, fun_name, fun_params, fun_body)

    let parse tokens = AST.Prog(parse_fun tokens)
end