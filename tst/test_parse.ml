open OUnit2

(* AST COMPARISON *)
let compare_ids (Ast.ID expected_id) (Ast.ID actual_id) = String.equal expected_id actual_id

let compare_params (Ast.Param(t1, id1)) (Ast.Param(t2, id2)) = 
    (t1 == t2) && (compare_ids id1 id2)

let compare_consts expected actual =
    match expected, actual with
    | Ast.Int(i1), Ast.Int(i2) -> i1 == i2
    | Ast.Char(c1), Ast.Char(c2) -> c1 == c2
    | _ -> false

let rec compare_exps expected actual =
    match expected, actual with
    | Ast.Const(expected_const), Ast.Const(actual_const) -> compare_consts expected_const actual_const
    | Ast.Var(id1), Ast.Var(id2) -> compare_ids id1 id2
    | Ast.BinOp(expected_op, expected_e1, expected_e2), Ast.BinOp(actual_op, actual_e1, actual_e2) -> 
        (expected_op == actual_op) && compare_exps expected_e1 actual_e1 && compare_exps expected_e2 actual_e2
    | Ast.UnOp(expected_op, expected_exp), Ast.UnOp(actual_op, actual_exp) ->
        (expected_op == actual_op) && compare_exps expected_exp actual_exp
    | Ast.FunCall(expected_id, expected_args), Ast.FunCall(actual_id, actual_args) ->
        compare_ids expected_id actual_id && List.for_all2 compare_exps expected_args actual_args
    | Ast.Assign(expected_op, expected_id, expected_exp), Ast.Assign(actual_op, actual_id, actual_exp) ->
        (expected_op == actual_op) && compare_ids expected_id actual_id && compare_exps expected_exp actual_exp
    | _ -> false

let compare_declarations 
    Ast.{ var_type = t1; var_name = id1; init = init1; }
    Ast.{ var_type = t2; var_name = id2; init = init2; } =
    t1 == t2 && compare_ids id1 id2 &&
        (match init1, init2 with
        | None, None -> true
        | Some e1, Some e2 -> compare_exps e1 e2
        | _ -> false)

let rec compare_statements expected actual = 
    match expected, actual with
    | Ast.Decl(decl1), Ast.Decl(decl2) -> compare_declarations decl1 decl2
    | Ast.ReturnVal(v1), Ast.ReturnVal(v2) -> compare_exps v1 v2
    | Ast.If(cond1, then1, else1), Ast.If(cond2, then2, else2) ->
        compare_exps cond1 cond2 && List.for_all2 compare_statements then1 then2 &&
        (match else1, else2 with
        | Some body1, Some body2 -> List.for_all2 compare_statements body1 body2
        | None, None -> true
        | _ -> false)
    | Ast.(For { init = init1; cond = cond1; post = post1; body = body1; },
        For { init = init2; cond = cond2; post = post2; body = body2; }) ->
        compare_exps init1 init2 && compare_exps cond1 cond2 && 
        compare_exps post1 post2 && List.for_all2 compare_statements body1 body2
    | Ast.(ForDecl { init = init1; cond = cond1; post = post1; body = body1; },
        ForDecl { init = init2; cond = cond2; post = post2; body = body2; }) ->
        compare_declarations init1 init2 && compare_exps cond1 cond2 && 
        compare_exps post1 post2 && List.for_all2 compare_statements body1 body2
    | Ast.Exp(e1), Ast.Exp(e2) -> compare_exps e1 e2
    | _ -> false

let compare_funs expected actual = 
    match expected, actual with
    | Ast.FunDecl(t1, name1, params1, Ast.Body(body1)), Ast.FunDecl(t2, name2, params2, Ast.Body(body2)) ->
    t1 == t2 && compare_ids name1 name2 && List.for_all2 compare_params params1 params2 && List.for_all2 compare_statements body1 body2

let compare_asts expected actual = 
    match expected, actual with
    | Ast.Prog(fun_list1), Ast.Prog(fun_list2) -> List.for_all2 compare_funs fun_list1 fun_list2


(* Test utilities *)

let print_test_failure expected actual =
    let _ = print_string "Expected AST:\n" in
    let _ = Pprint.pprint expected in
    let _ = print_string "Actual AST:\n" in
    let _ = Pprint.pprint actual in
    ()

let test_expect_failure input fail_msg test_ctxt =
    let f =  fun () -> Parse.parse input in
    assert_raises (Failure fail_msg) f

let test_compare_asts tokens expected_ast test_ctxt =
    let actual_ast = Parse.parse tokens in
    let same = compare_asts expected_ast actual_ast in
    let _  = 
        if (not same)
        then (print_test_failure expected_ast actual_ast)
        else () in
    assert_bool "Mismatched ASTS" same

(* ast with multiple statements *)
let make_ast params statements =
    let body = Ast.Body(statements) in
    let fun_name = Ast.ID("main") in
    let f = Ast.FunDecl(Ast.IntType, fun_name, params, body) in
    Ast.Prog([f])

let make_simple_ast params exp =
    let ret = Ast.ReturnVal(exp) in
    make_ast params [ret]       

(* CONSTANTS *)

let simple_token_list = Lex.lex "int main(){return 2;}"
let simple_ast = make_simple_ast [] (Ast.Const(Ast.Int(2)))

(* TODO: this probably belongs in a different test group *)
let fun_arg_token_list = Lex.lex "int main(int argc){return 2;}"
let fun_arg_ast =
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let return_exp = Ast.Const(Ast.Int(2)) in
    make_simple_ast params return_exp

let return_char_tokens = Lex.lex "int main(int argc){return 'a';}"
let return_char_ast =
    let params = [Ast.Param(Ast.IntType, Ast.ID("argc"))] in
    let return_exp = Ast.Const(Ast.Char('a')) in
    make_simple_ast params return_exp

let basic_parse_tests = [
    "test_simple_parse" >:: test_compare_asts simple_token_list simple_ast;
    "test_fun_args" >:: test_compare_asts fun_arg_token_list fun_arg_ast;
    "test_return_char" >:: test_compare_asts return_char_tokens return_char_ast;
]

(* UNARY OPERATORS *)

let negation_tokens = Lex.lex "int main(){ return -3;}"
let negation_ast =
    let unop = Ast.UnOp(Ast.Negate, Ast.Const(Ast.Int(3))) in
    make_simple_ast [] unop

let pos_tokens = Lex.lex "int main() { return +3;}"
let pos_ast =
    let unop = Ast.UnOp(Ast.Pos, Ast.Const(Ast.Int(3))) in
    make_simple_ast [] unop

let complement_tokens = Lex.lex "int main() {return ~3;}"
let complement_ast = 
    let unop = Ast.UnOp(Ast.Complement, Ast.Const(Ast.Int(3))) in
    make_simple_ast [] unop

let not_tokens = Lex.lex "int main() {return !4;}"
let not_ast =
    let unop = Ast.UnOp(Ast.Not, Ast.Const(Ast.Int(4))) in
    make_simple_ast [] unop

let unop_parse_tests = [
    "test_negation" >:: test_compare_asts negation_tokens negation_ast;
    "test_pos" >:: test_compare_asts pos_tokens pos_ast;
    "test_complement" >:: test_compare_asts complement_tokens complement_ast;
    "test_not" >:: test_compare_asts not_tokens not_ast;
]

(* BINARY OPERATORS *)

let addition_tokens = Lex.lex "int main(){return 1+2;}"
let addition_ast = 
    let binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let params = [] in
    make_simple_ast params binop

let subtraction_tokens = Lex.lex "int main(){return 4-3;}"
let subtraction_ast =
    let binop = Ast.BinOp(Ast.Sub, Ast.Const(Ast.Int(4)), Ast.Const(Ast.Int(3))) in
    make_simple_ast [] binop

let subtract_negative_tokens = Lex.lex "int main(){return 4- -3;}"
let subtract_negative_ast =
    let unop = Ast.UnOp(Ast.Negate, Ast.Const(Ast.Int(3))) in
    let binop = Ast.BinOp(Ast.Sub, Ast.Const(Ast.Int(4)), unop) in
    make_simple_ast [] binop

let multi_addition_tokens = Lex.lex "int main() {return 1+2+3;}"
let multi_addition_ast =
    (* NOTE: this is actually wrong, addition in C is left-associative *)
    let inner_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Add, inner_binop, Ast.Const(Ast.Int(3))) in
    let params = [] in
    make_simple_ast params outer_binop

let division_tokens = Lex.lex "int main() {return 4/5;}"
let division_ast =
    let binop = Ast.BinOp(Ast.Div, Ast.Const(Ast.Int(4)), Ast.Const(Ast.Int(5))) in
    make_simple_ast [] binop

let nested_addition_tokens = Lex.lex "int main(){return 1+(2+3);}"
let nested_addition_ast =
    let inner_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(2)), Ast.Const(Ast.Int(3))) in
    let outer_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), inner_binop) in
    make_simple_ast [] outer_binop

let lots_of_parens_tokens = Lex.lex "int main(){return ((3));}"
let lots_of_parens_ast = make_simple_ast [] (Ast.Const(Ast.Int(3)))

let left_nested_addition_tokens = Lex.lex "int main() {return (1+2)+3;}"
let left_nested_addition_ast =
    let inner_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Add, inner_binop, Ast.Const(Ast.Int(3))) in
    let params = [] in
    make_simple_ast params outer_binop

let mult_tokens = Lex.lex "int main() {return 4*5;}"
let mult_ast = 
    let binop = Ast.BinOp(Ast.Mult, Ast.Const(Ast.Int(4)), Ast.Const(Ast.Int(5))) in
    make_simple_ast [] binop

let lots_of_parens_add_tokens = Lex.lex "int main(){return ((1+2));}"
let lots_of_parens_add_ast = 
    let e1 = Ast.Const(Ast.Int(1)) in
    let e2 = Ast.Const(Ast.Int(2)) in
    let ret_exp = Ast.BinOp(Ast.Add, e1, e2) in
    make_simple_ast [] ret_exp

let mod_toks = Lex.lex "int main() {return 3%2;}"
let mod_ast =
    let e1 = Ast.Const(Ast.Int(3)) in
    let e2 = Ast.Const(Ast.Int(2)) in
    let ret_exp = Ast.BinOp(Ast.Mod, e1, e2) in
    make_simple_ast [] ret_exp

let precedence_tokens = Lex.lex "int main() {return 1*2+3%2;}"
let precedence_ast = 
    let e1 = Ast.BinOp(Ast.Mult, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let e2 = Ast.BinOp(Ast.Mod, Ast.Const(Ast.Int(3)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Add, e1, e2) in
    make_simple_ast [] outer_binop

let associativity_tokens = Lex.lex "int main() {return 1/2*3;}"
let associativity_ast =
    let inner_binop = Ast.BinOp(Ast.Div, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    let outer_binop = Ast.BinOp(Ast.Mult, inner_binop, Ast.Const(Ast.Int(3))) in
    make_simple_ast [] outer_binop

let binop_parse_tests = [
    "test_addition" >:: test_compare_asts addition_tokens addition_ast;
    "test_subtraction" >:: test_compare_asts subtraction_tokens subtraction_ast;
    "test_subtract_negative" >:: test_compare_asts subtract_negative_tokens subtract_negative_ast;
    "test_multiplication" >:: test_compare_asts mult_tokens mult_ast;
    "test_division" >:: test_compare_asts division_tokens division_ast;
    "test_mod" >:: test_compare_asts mod_toks mod_ast;
    "test_nested_addition" >:: test_compare_asts nested_addition_tokens nested_addition_ast;
    "test_lots_of_parens" >:: test_compare_asts lots_of_parens_tokens lots_of_parens_ast;
    "test_left_nested_addition" >:: test_compare_asts left_nested_addition_tokens left_nested_addition_ast;
    "test_lots_of_parens_add" >:: test_compare_asts lots_of_parens_add_tokens lots_of_parens_add_ast;
    "test_precedence" >:: test_compare_asts precedence_tokens precedence_ast;
    "test_associativity" >:: test_compare_asts associativity_tokens associativity_ast;
]

(* BITWISE BINOPS *)

let bitwise_and_tokens = Lex.lex "int main() {return 1&2;}"
let bitwise_and_ast =
    let binop = Ast.BinOp(Ast.BitAnd, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let bitwise_or_tokens = Lex.lex "int main() {return 1|2;}"
let bitwise_or_ast =
    let binop = Ast.BinOp(Ast.BitOr, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let xor_tokens = Lex.lex "int main() {return 1^2;}"
let xor_ast =
    let binop = Ast.BinOp(Ast.Xor, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let shiftl_tokens = Lex.lex "int main() {return 1<<2;}"
let shiftl_ast =
    let binop = Ast.BinOp(Ast.ShiftL, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let shiftr_tokens = Lex.lex "int main() {return 1>>2;}"
let shiftr_ast =
    let binop = Ast.BinOp(Ast.ShiftR, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let bitwise_binops_tests = [
    "test_bitwise_and" >:: test_compare_asts bitwise_and_tokens bitwise_and_ast;
    "test_bitwise_or" >:: test_compare_asts bitwise_or_tokens bitwise_or_ast;
    "test_xor" >:: test_compare_asts xor_tokens xor_ast;
    "test_shiftl" >:: test_compare_asts shiftl_tokens shiftl_ast;
    "test_shiftr" >:: test_compare_asts shiftr_tokens shiftr_ast
]

(* BOOLEAN BINOPS *)

let and_tokens = Lex.lex "int main() {return 1&&2;}"
let and_ast =
    let binop = Ast.BinOp(Ast.And, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let or_tokens = Lex.lex "int main() {return 1||2;}"
let or_ast =
    let binop = Ast.BinOp(Ast.Or, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

(* addition is higher precedence than &&, which is higher precedence than || *)
let bool_precedence_tokens = Lex.lex "int main() {return 1 || 2 && 3 + 4;}"
let bool_precedence_ast =
    let add_binop = Ast.BinOp(Ast.Add, Ast.Const(Ast.Int(3)), Ast.Const(Ast.Int(4))) in
    let and_binop = Ast.BinOp(Ast.And, Ast.Const(Ast.Int(2)), add_binop) in
    let or_binop = Ast.BinOp(Ast.Or, Ast.Const(Ast.Int(1)), and_binop) in
    make_simple_ast [] or_binop

let boolean_binop_tests = [
    "test_and" >:: test_compare_asts and_tokens and_ast;
    "test_or" >:: test_compare_asts or_tokens or_ast;
    "test_bool_precedence" >:: test_compare_asts bool_precedence_tokens bool_precedence_ast;
]


(* COMPARISON OPERATORS *)

let compare_eq_tokens = Lex.lex "int main() {return 1==2;}"
let compare_eq_ast =
    let binop = Ast.BinOp(Ast.Eq, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let compare_neq_tokens = Lex.lex "int main() {return 1!=2;}"
let compare_neq_ast =
    let binop = Ast.BinOp(Ast.Neq, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let compare_gt_tokens = Lex.lex "int main() {return 1 > 2;}"
let compare_gt_ast =
    let binop = Ast.BinOp(Ast.Gt, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let compare_ge_tokens = Lex.lex "int main() {return 1 >= 2;}"
let compare_ge_ast =
    let binop = Ast.BinOp(Ast.Ge, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let compare_lt_tokens = Lex.lex "int main() {return 1 < 2;}"
let compare_lt_ast =
    let binop = Ast.BinOp(Ast.Lt, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let compare_le_tokens = Lex.lex "int main() {return 1 <= 2;}"
let compare_le_ast =
    let binop = Ast.BinOp(Ast.Le, Ast.Const(Ast.Int(1)), Ast.Const(Ast.Int(2))) in
    make_simple_ast [] binop

let comp_parse_tests = [
    "test_eq" >:: test_compare_asts compare_eq_tokens compare_eq_ast;
    "test_neq" >:: test_compare_asts compare_neq_tokens compare_neq_ast;
    "test_lt" >:: test_compare_asts compare_lt_tokens compare_lt_ast;
    "test_gt" >:: test_compare_asts compare_gt_tokens compare_gt_ast;
    "test_le" >:: test_compare_asts compare_le_tokens compare_le_ast;
    "test_ge" >:: test_compare_asts compare_ge_tokens compare_ge_ast;    
]

(* VARIABLES *)

let declaration_tokens = Lex.lex "int main(){int a=2; return a;}"
let declaration_ast =
    let decl = Ast.(Decl { var_type = IntType;
                           var_name = Ast.ID("a");
                           init = Some(Const(Int 2));
                          })
    in
    let ret = Ast.ReturnVal(Ast.Var(Ast.ID("a"))) in
    let statements = [decl; ret] in
    make_ast [] statements

let assignment_tokens = Lex.lex "int main(){int a; a=2; return 0;}"
let assignment_ast =
    let decl = Ast.(Decl { var_type = IntType;
                           var_name = Ast.ID("a");
                           init = None;
        }) 
    in
    let assign = Ast.Exp(Ast.Assign(Ast.Equals, Ast.ID("a"), Ast.Const(Ast.Int(2)))) in
    let ret = Ast.ReturnVal(Ast.Const(Ast.Int(0))) in
    let statements = [decl; assign; ret] in
    make_ast [] statements 

let multi_assign_tokens = Lex.lex "int main(){int a; int b = a = 2; return b;}"
let multi_assign_ast =
    let decl_1 = Ast.(Decl { var_type = IntType;
                             var_name = Ast.ID("a");
                             init = None
                         }) in
    let assign = Ast.Assign(Ast.Equals, Ast.ID("a"), Ast.Const(Ast.Int(2))) in
    let decl_2 = Ast.(Decl { var_type = IntType;
                             var_name = Ast.ID("b");
                             init = Some(assign)
                         }) in
    let ret = Ast.ReturnVal(Ast.Var(Ast.ID("b"))) in
    let statements = [decl_1; decl_2; ret] in
    make_ast [] statements


let variable_parse_tests = [
    "test_declaration" >:: test_compare_asts declaration_tokens declaration_ast;
    "test_assignment" >:: test_compare_asts assignment_tokens assignment_ast;
    "test_multi_assign" >:: test_compare_asts multi_assign_tokens multi_assign_ast;
]

(* CONDITIONALS *)

let single_if_tokens = Lex.lex "int main(){if (0) return 1; return 0;}"
let single_if_ast =
    let if_condition = Ast.Const(Ast.Int(0)) in
    let if_body = [Ast.ReturnVal(Ast.Const(Ast.Int(1)))] in
    let if_statement = Ast.If(if_condition, if_body, None) in
    let return = Ast.ReturnVal(Ast.Const(Ast.Int(0))) in
    make_ast [] [if_statement; return]

let single_if_else_tokens = Lex.lex "int main(){if (0) return 1; else return 2;}"
let single_if_else_ast =
    let if_condition = Ast.Const(Ast.Int(0)) in
    let if_body = [Ast.ReturnVal(Ast.Const(Ast.Int(1)))] in
    let else_body =  [Ast.ReturnVal(Ast.Const(Ast.Int(2)))] in
    let if_statement = Ast.If(if_condition, if_body, Some(else_body)) in
    make_ast [] [if_statement]

let conditional_parse_tests = [
    "test_single_if" >:: test_compare_asts single_if_tokens single_if_ast;
    "test_single_if_else" >:: test_compare_asts single_if_else_tokens single_if_else_ast;
]

(* FOR LOOPS *)

let for_tokens = Lex.lex "int main() {for(1; 1; 1) { 1;}}"
let for_ast =
    let const = Ast.Const(Ast.Int(1)) in
    let for_loop = Ast.For { init=const; cond=const; post=const; body=[Ast.Exp const] } in
    make_ast [] [for_loop]

let for_compound_tokens = Lex.lex "int main() {int a; for(a=0; a<5; a=a+1){ 1+1; if(a<5) {return 3;} } }"
let for_compound_ast =
    let open Ast in
    let decl = Decl { var_type = IntType; var_name = ID("a"); init = None } in
    let init = Assign(Equals, ID("a"), Const(Int 0)) in
    let post = Assign(Equals, ID("a"), BinOp(Add, Var(ID "a"), Const (Int 1))) in
    let cond = BinOp(Lt, Var(ID("a")), Const(Int 5)) in
    let exp = Exp(BinOp(Add, Const(Int 1), Const(Int 1))) in
    let return = ReturnVal(Const(Int 3)) in
    let if_statement = If(cond, [return], None) in
    let body = [exp; if_statement] in
    let for_loop = For { init; cond; post; body } in
    make_ast [] [decl; for_loop]

let for_declaration_tokens = Lex.lex "int main() {for(int a; 1; 1) { 1;}}"
let for_decl_ast =
    let const = Ast.Const(Ast.Int(1)) in
    let decl = Ast.({ var_type = IntType; var_name = ID("a"); init = None}) in
    let for_loop = Ast.ForDecl { init=decl; cond=const; post=const; body=[Ast.Exp const] } in
    make_ast [] [for_loop]

let for_parse_tests = [
    "test_for" >:: test_compare_asts for_tokens for_ast;
    "test_for_compound" >:: test_compare_asts for_compound_tokens for_compound_ast;
    "test_for_declaration" >:: test_compare_asts for_declaration_tokens for_decl_ast;
]

(* FUNCTION CALLS *)
let fun_tokens = Lex.lex "int main() { return foo(); }" (* note: call to undefined function should fail during code generation, not parsing *)
let fun_ast =
    let fun_call = Ast.FunCall(Ast.ID("foo"), []) in
    make_simple_ast [] fun_call

let fun_args_tokens = Lex.lex "int main() { return foo(5); }"
let fun_args_ast =
    let fun_call = Ast.FunCall(Ast.ID("foo"), [Ast.Const(Ast.Int(5))]) in
    make_simple_ast [] fun_call

let fun_arg_exp_tokens = Lex.lex "int main() { return foo(a + 5); }"
let fun_arg_exp_ast =
    let arg_exp = Ast.BinOp(Ast.Add, Ast.Var(Ast.ID("a")), Ast.Const(Ast.Int(5))) in
    let fun_call = Ast.FunCall(Ast.ID("foo"), [arg_exp]) in
    make_simple_ast [] fun_call

let fun_call_standalone = "int main() {incr (b);}"

let fun_call_parse_tests = [
    "test_simple_call" >:: test_compare_asts fun_tokens fun_ast;
    "test_call_with_args" >:: test_compare_asts fun_args_tokens fun_args_ast;
    "test_call_with_complex_arg" >:: test_compare_asts fun_arg_exp_tokens fun_arg_exp_ast;
]

(* FAILURE *)

let bad_token_list = [Tok.IntKeyword]
let missing_semicolon = Lex.lex "int main(){return 2}"

let incomplete_addition = Lex.lex "int main(){return 2+;}"
let mismatched_parens = Lex.lex "int main() {return ((1);}"
let mismatched_right_parens = Lex.lex "int main() {return (1));}"
let one_paren = Lex.lex "int main() {return (1;}"
let backwards_parens = Lex.lex "int main() {return )1+2;}"

let failure_parse_tests = [
    "test_parse_fail" >:: test_expect_failure bad_token_list "Parse error in parse_fun: bad function type or name";
    "test_semicolon_required" >:: test_expect_failure missing_semicolon "Expected semicolon at end of statement";
    "test_incomplete_addition" >:: test_expect_failure incomplete_addition "Failed to parse factor";
    "test_mismatched_parens" >:: test_expect_failure mismatched_parens "Syntax error: expected close paren";
    "test_mismatched_right_parens" >:: test_expect_failure mismatched_right_parens "Expected semicolon at end of statement";    
    "test_one_paren" >:: test_expect_failure one_paren "Syntax error: expected close paren";
    "test_backwards_parens" >:: test_expect_failure backwards_parens "Failed to parse factor";
]

(* TODO: add comp parse test *)
let parse_tests = basic_parse_tests@unop_parse_tests@binop_parse_tests@boolean_binop_tests
                @bitwise_binops_tests@comp_parse_tests@variable_parse_tests
                @conditional_parse_tests@fun_call_parse_tests@for_parse_tests
                @failure_parse_tests