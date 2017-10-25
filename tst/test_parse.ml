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

let compare_ops expected actual =
    match expected, actual with 
    | Ast.Add, Ast.Add -> true

let rec compare_exps expected actual =
    match expected, actual with
    | Ast.Const(expected_const), Ast.Const(actual_const) -> compare_consts expected_const actual_const
    | Ast.BinOp(expected_op, expected_e1, expected_e2), Ast.BinOp(actual_op, actual_e1, actual_e2) -> 
        compare_ops expected_op actual_op && compare_exps expected_e1 actual_e1 && compare_exps expected_e2 actual_e2
    | _ -> false

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

let make_ast params exp =
    let ret = Ast.ReturnVal(exp) in
    let body = Ast.Body([ret]) in
    let fun_name = Ast.ID("main") in
    let f = Ast.FunDecl(Ast.IntType, fun_name, params, body) in
    Ast.Prog(f)

let simple_token_list = Lex.lex "int main(){return 2}"
let simple_ast = make_ast [] (Ast.Const(Ast.Int(2)))

let fun_arg_token_list = Lex.lex "int main(int argc){return 2;}"
let fun_arg_ast =
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let return_exp = Ast.Const(Ast.Int(2)) in
    make_ast params return_exp

let return_char_tokens = Lex.lex "int main(int argc){return 'a'}"
let return_char_ast =
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let return_exp = Ast.Const(Ast.Char('a')) in
    make_ast params return_exp

let addition_tokens = Lex.lex "int main(){return 1+2;}"

let addition_ast = 
    let binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let params = [] in
    make_ast params binop

let multi_addition_tokens = Lex.lex "int main() {return 1+2+3;}"
let multi_addition_ast =
    let inner_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(2)), Ast.Const(Ast.Int(3))) in
    let outer_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), inner_binop) in
    let params = [] in
    make_ast params outer_binop

let nested_addition_tokens = Lex.lex "int main(){return 1+(2+3);}"
let nested_addition_ast = multi_addition_ast

let bad_token_list = [Tok.IntKeyword]

let parse_tests = [
    "test_simple_parse" >:: test_compare_asts simple_token_list simple_ast;
    "test_fun_args" >:: test_compare_asts fun_arg_token_list fun_arg_ast;
    "test_return_char" >:: test_compare_asts return_char_tokens return_char_ast;
    "test_addition" >:: test_compare_asts addition_tokens addition_ast;
    "test_nested_addition" >:: test_compare_asts nested_addition_tokens nested_addition_ast;

    "test_parse_fail" >:: test_expect_failure bad_token_list "Parse error in parse_fun: bad function type or name"
]