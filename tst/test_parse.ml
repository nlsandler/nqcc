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

let rec compare_exps expected actual =
    match expected, actual with
    | Ast.Const(expected_const), Ast.Const(actual_const) -> compare_consts expected_const actual_const
    | Ast.BinOp(expected_op, expected_e1, expected_e2), Ast.BinOp(actual_op, actual_e1, actual_e2) -> 
        (expected_op == actual_op) && compare_exps expected_e1 actual_e1 && compare_exps expected_e2 actual_e2
    | Ast.UnOp(expected_op, expected_exp), Ast.UnOp(actual_op, actual_exp) ->
        (expected_op == actual_op) && compare_exps expected_exp actual_exp
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

let simple_token_list = Lex.lex "int main(){return 2;}"
let simple_ast = make_ast [] (Ast.Const(Ast.Int(2)))

let fun_arg_token_list = Lex.lex "int main(int argc){return 2;}"
let fun_arg_ast =
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let return_exp = Ast.Const(Ast.Int(2)) in
    make_ast params return_exp

let return_char_tokens = Lex.lex "int main(int argc){return 'a';}"
let return_char_ast =
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let return_exp = Ast.Const(Ast.Char('a')) in
    make_ast params return_exp

let negation_tokens = Lex.lex "int main(){ return -3;}"
let negation_ast =
    let unop = Ast.UnOp(Ast.Negate, Ast.Const(Ast.Int(3))) in
    make_ast [] unop

let pos_tokens = Lex.lex "int main() { return +3;}"
let pos_ast =
    let unop = Ast.UnOp(Ast.Pos, Ast.Const(Ast.Int(3))) in
    make_ast [] unop

let complement_tokens = Lex.lex "int main() {return ~3;}"
let complement_ast = 
    let unop = Ast.UnOp(Ast.Complement, Ast.Const(Ast.Int(3))) in
    make_ast [] unop

let not_tokens = Lex.lex "int main() {return !4;}"
let not_ast =
    let unop = Ast.UnOp(Ast.Not, Ast.Const(Ast.Int(4))) in
    make_ast [] unop

let addition_tokens = Lex.lex "int main(){return 1+2;}"
let addition_ast = 
    let binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let params = [] in
    make_ast params binop

let subtraction_tokens = Lex.lex "int main(){return 4-3;}"
let subtraction_ast =
    let binop = Ast.BinOp(Ast.Sub, Ast.Const(Ast.Int(4)), Ast.Const(Ast.Int(3))) in
    make_ast [] binop

let subtract_negative_tokens = Lex.lex "int main(){return 4- -3;}"
let subtract_negative_ast =
    let unop = Ast.UnOp(Ast.Negate, Ast.Const(Ast.Int(3))) in
    let binop = Ast.BinOp(Ast.Sub, Ast.Const(Ast.Int(4)), unop) in
    make_ast [] binop

let multi_addition_tokens = Lex.lex "int main() {return 1+2+3;}"
let multi_addition_ast =
    (* NOTE: this is actually wrong, addition in C is left-associative *)
    let inner_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Add, inner_binop, Ast.Const(Ast.Int(3))) in
    let params = [] in
    make_ast params outer_binop

let division_tokens = Lex.lex "int main() {return 4/5;}"
let division_ast =
    let binop = Ast.BinOp(Ast.Div, Ast.Const(Ast.Int(4)), Ast.Const(Ast.Int(5))) in
    make_ast [] binop

let nested_addition_tokens = Lex.lex "int main(){return 1+(2+3);}"
let nested_addition_ast =
    let inner_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(2)), Ast.Const(Ast.Int(3))) in
    let outer_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), inner_binop) in
    make_ast [] outer_binop

let lots_of_parens_tokens = Lex.lex "int main(){return ((3));}"
let lots_of_parens_ast = make_ast [] (Ast.Const(Ast.Int(3)))

let left_nested_addition_tokens = Lex.lex "int main() {return (1+2)+3;}"
let left_nested_addition_ast =
    let inner_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Add, inner_binop, Ast.Const(Ast.Int(3))) in
    let params = [] in
    make_ast params outer_binop

let mult_tokens = Lex.lex "int main() {return 4*5;}"
let mult_ast = 
    let binop = Ast.BinOp(Ast.Mult, Ast.Const(Ast.Int(4)), Ast.Const(Ast.Int(5))) in
    make_ast [] binop

let lots_of_parens_add_tokens = Lex.lex "int main(){return ((1+2));}"
let lots_of_parens_add_ast = 
    let e1 = Ast.Const(Ast.Int(1)) in
    let e2 = Ast.Const(Ast.Int(2)) in
    let ret_exp = Ast.BinOp(Ast.Add, e1, e2) in
    make_ast [] ret_exp

let precedence_tokens = Lex.lex "int main() {return 1*2+3;}"
let precedence_ast = 
    let inner_binop = Ast.BinOp(Ast.Mult, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Add, inner_binop, Ast.Const(Ast.Int(3))) in
    make_ast [] outer_binop

let associativity_tokens = Lex.lex "int main() {return 1/2*3;}"
let associativity_ast =
    let inner_binop = Ast.BinOp(Ast.Div, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Mult, inner_binop, Ast.Const(Ast.Int(3))) in
    make_ast [] outer_binop

let bad_token_list = [Tok.IntKeyword]
let missing_semicolon = Lex.lex "int main(){return 2}"

let incomplete_addition = Lex.lex "int main(){return 2+;}"
let mismatched_parens = Lex.lex "int main() {return ((1);}"
let mismatched_right_parens = Lex.lex "int main() {return (1));}"
let one_paren = Lex.lex "int main() {return (1;}"
let backwards_parens = Lex.lex "int main() {return )1+2;}"

let parse_tests = [
    "test_simple_parse" >:: test_compare_asts simple_token_list simple_ast;
    "test_fun_args" >:: test_compare_asts fun_arg_token_list fun_arg_ast;
    "test_return_char" >:: test_compare_asts return_char_tokens return_char_ast;
    "test_negation" >:: test_compare_asts negation_tokens negation_ast;
    "test_pos" >:: test_compare_asts pos_tokens pos_ast;
    "test_complement" >:: test_compare_asts complement_tokens complement_ast;
    "test_not" >:: test_compare_asts not_tokens not_ast;
    "test_addition" >:: test_compare_asts addition_tokens addition_ast;
    "test_subtraction" >:: test_compare_asts subtraction_tokens subtraction_ast;
    "test_subtract_negative" >:: test_compare_asts subtract_negative_tokens subtract_negative_ast;
    "test_multiplication" >:: test_compare_asts mult_tokens mult_ast;
    "test_division" >:: test_compare_asts division_tokens division_ast;
    "test_nested_addition" >:: test_compare_asts nested_addition_tokens nested_addition_ast;
    "test_lots_of_parens" >:: test_compare_asts lots_of_parens_tokens lots_of_parens_ast;
    "test_left_nested_addition" >:: test_compare_asts left_nested_addition_tokens left_nested_addition_ast;
    "test_lots_of_parens_add" >:: test_compare_asts lots_of_parens_add_tokens lots_of_parens_add_ast;
    "test_precedence" >:: test_compare_asts precedence_tokens precedence_ast;
    "test_associativity" >:: test_compare_asts associativity_tokens associativity_ast;
    "test_parse_fail" >:: test_expect_failure bad_token_list "Parse error in parse_fun: bad function type or name";
    "test_semicolon_required" >:: test_expect_failure missing_semicolon "Expected semicolon at end of statement";
    "test_incomplete_addition" >:: test_expect_failure incomplete_addition "Failed to parse factor";
    "test_mismatched_parens" >:: test_expect_failure mismatched_parens "Syntax error: expected close paren";
    "test_mismatched_right_parens" >:: test_expect_failure mismatched_right_parens "Expected semicolon at end of statement";    
    "test_one_paren" >:: test_expect_failure one_paren "Syntax error: expected close paren";
    "test_backwards_parens" >:: test_expect_failure backwards_parens "Failed to parse factor";
]