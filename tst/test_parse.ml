open OUnit2

(* AST COMPARISON *)
let compare_ids (Ast.ID expected_id) (Ast.ID actual_id) = String.equal expected_id actual_id

let compare_types expected actual =
    match (expected, actual) with
    | Ast.IntType, Ast.IntType -> true
    | Ast.CharType, Ast.CharType -> true
    | _ -> false

let compare_params (Ast.Param(t1, id1)) (Ast.Param(t2, id2)) = 
    (compare_types t1 t2) && (compare_ids id1 id2)

let rec compare_param_lists expected actual = 
    match expected, actual with
    | [], [] -> true
    | p1::ps1, p2::ps2 -> (compare_params p1 p2)&&(compare_param_lists ps1 ps1)
    | _ -> false

let compare_consts expected actual =
    match expected, actual with
    | Ast.Int(i1), Ast.Int(i2) -> i1 == i2
    | Ast.Char(c1), Ast.Char(c2) -> c1 == c2
    | _ -> false

let compare_exps expected actual =
    match expected, actual with
    | Ast.Const(expected_const), Ast.Const(actual_const) -> compare_consts expected_const actual_const

let compare_statements expected actual = 
    match expected, actual with
    | Ast.Return, Ast.Return -> true
    | Ast.ReturnVal(v1), Ast.ReturnVal(v2) -> compare_exps v1 v2
    | _ -> false

let rec compare_statement_lists expected actual =
    match expected, actual with
    | [], [] -> true
    | stmt1::stmts1, stmt2::stmts2 -> (compare_statements stmt1 stmt2) && compare_statement_lists stmts1 stmts2
    | _ -> false

let compare_funs expected actual = 
    match expected, actual with
    | Ast.FunDecl(t1, name1, params1, Ast.Body(body1)), Ast.FunDecl(t2, name2, params2, Ast.Body(body2)) ->
    compare_types t1 t2 && compare_ids name1 name2 && compare_param_lists params1 params2 && compare_statement_lists body1 body2

let compare_asts expected actual = 
    match expected, actual with
    | Ast.Prog(fun1), Ast.Prog(fun2) -> compare_funs fun1 fun2


(* Actual tests *)
(*
let print_test_failure expected actual =
    let _ = print_string "Expected AST:\n" in
    let _ = Pprint.pprint expected in
    let _ = print_string "Actual AST:\n" in
    let _ = Pprint.pprint actual in
    "Mismatched AST"
*)
let test_expect_failure input fail_msg test_ctxt =
    let f =  fun () -> Parse.parse input in
    assert_raises (Failure fail_msg) f

let test_compare_asts tokens expected_ast test_ctxt =
    let actual_ast = Parse.parse tokens in
    assert_bool "Mismatched ASTs" (compare_asts expected_ast actual_ast)

let simple_token_list = [Lex.IntKeyword; Lex.Id("main"); Lex.OpenParen; Lex.CloseParen; Lex.OpenBrace; Lex.ReturnKeyword; Lex.Int(2); Lex.CloseBrace]
let simple_ast = 
    let ret = Ast.ReturnVal(Ast.Const(Ast.Int(2))) in
    let body = Ast.Body([ret]) in
    let params = [] in
    let fun_name = Ast.ID("main") in
    let simple_fun = Ast.FunDecl(Ast.IntType, fun_name, params, body) in
    Ast.Prog(simple_fun)

let fun_arg_token_list = [Lex.IntKeyword; Lex.Id("main"); 
                        Lex.OpenParen; Lex.IntKeyword; Lex.Id("argc"); Lex.CloseParen; 
                        Lex.OpenBrace; Lex.ReturnKeyword; Lex.Int(2); Lex.CloseBrace]
let fun_arg_ast = 
    let ret = Ast.ReturnVal(Ast.Const(Ast.Int(2))) in
    let body = Ast.Body([ret]) in
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let fun_name = Ast.ID("main") in
    let simple_fun = Ast.FunDecl(Ast.IntType, fun_name, params, body) in
    Ast.Prog(simple_fun)

let return_char_tokens = [Lex.IntKeyword; Lex.Id("main"); 
                        Lex.OpenParen; Lex.IntKeyword; Lex.Id("argc"); Lex.CloseParen; 
                        Lex.OpenBrace; Lex.ReturnKeyword; Lex.Char('a'); Lex.CloseBrace]

let return_char_ast = 
    let ret = Ast.ReturnVal(Ast.Const(Ast.Char('a'))) in
    let body = Ast.Body([ret]) in
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let fun_name = Ast.ID("main") in
    let simple_fun = Ast.FunDecl(Ast.IntType, fun_name, params, body) in
    Ast.Prog(simple_fun)

let bad_token_list = [Lex.IntKeyword]

let parse_tests = [
    "test_simple_parse" >:: test_compare_asts simple_token_list simple_ast;
    "test_fun_args" >:: test_compare_asts fun_arg_token_list fun_arg_ast;
    "test_return_char" >:: test_compare_asts return_char_tokens return_char_ast;
    "test_parse_fail" >:: test_expect_failure bad_token_list "Parse error in parse_fun: bad function type or name"
]