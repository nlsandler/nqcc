open OUnit2
open Ast

(* AST COMPARISON *)
let compare_ids (ID expected_id) (ID actual_id) = String.equal expected_id actual_id

let compare_params (Param (t1, id1)) (Param (t2, id2)) =
  (t1 == t2) && (compare_ids id1 id2)

let compare_consts expected actual =
  match expected, actual with
  | Int i1, Int i2 -> i1 == i2
  | Char c1, Char c2 -> c1 == c2
  | _ -> false

let rec compare_exps expected actual =
  match expected, actual with
  | Const expected_const, Const actual_const -> compare_consts expected_const actual_const
  | Var id1, Var id2 -> compare_ids id1 id2
  | BinOp (expected_op, expected_e1, expected_e2), BinOp (actual_op, actual_e1, actual_e2) ->
     (expected_op == actual_op) && compare_exps expected_e1 actual_e1 && compare_exps expected_e2 actual_e2
  | UnOp (expected_op, expected_exp), UnOp (actual_op, actual_exp) ->
     (expected_op == actual_op) && compare_exps expected_exp actual_exp
  | FunCall (expected_id, expected_args), FunCall (actual_id, actual_args) ->
     compare_ids expected_id actual_id && List.for_all2 compare_exps expected_args actual_args
  | Assign (expected_op, expected_id, expected_exp), Assign (actual_op, actual_id, actual_exp) ->
     (expected_op == actual_op) && compare_ids expected_id actual_id && compare_exps expected_exp actual_exp
  | TernOp (expected_1, expected_2, expected_3), TernOp (actual_1, actual_2, actual_3) ->
     compare_exps expected_1 actual_1 && compare_exps expected_2 actual_2 && compare_exps expected_3 actual_3
  | _ -> false

let compare_declarations
      { var_type = t1; var_name = id1; init = init1; }
      { var_type = t2; var_name = id2; init = init2; } =
  t1 == t2 && compare_ids id1 id2 &&
    match init1, init2 with
     | None, None -> true
     | Some e1, Some e2 -> compare_exps e1 e2
     | _ -> false

let rec compare_statements expected actual =
  match expected, actual with
  | ReturnVal v1, ReturnVal v2 -> compare_exps v1 v2
  | Block stmts1, Block stmts2 -> List.for_all2 compare_block_items stmts1 stmts2
  | If { cond=cond1; if_body=then1; else_body=else1 },
    If { cond=cond2; if_body=then2; else_body=else2 } ->
     compare_exps cond1 cond2 && compare_statements then1 then2 &&
       (match else1, else2 with
        | Some body1, Some body2 -> compare_statements body1 body2
        | None, None -> true
        | _ -> false)
  | For { init = init1; cond = cond1; post = post1; body = body1; },
    For { init = init2; cond = cond2; post = post2; body = body2; } ->
     compare_exps init1 init2 && compare_exps cond1 cond2 &&
       compare_exps post1 post2 && compare_statements body1 body2
  | ForDecl { init = init1; cond = cond1; post = post1; body = body1; },
    ForDecl { init = init2; cond = cond2; post = post2; body = body2; } ->
     compare_declarations init1 init2 && compare_exps cond1 cond2 &&
       compare_exps post1 post2 && compare_statements body1 body2
  | While { cond = cond1; body = body1 },
    While { cond = cond2; body = body2 } ->
     compare_exps cond1 cond2 && compare_statements body1 body2
  | DoWhile { cond = cond1; body = body1 },
    DoWhile { cond = cond2; body = body2 } ->
     compare_exps cond1 cond2 && compare_statements body1 body2
  | Exp e1, Exp e2 -> compare_exps e1 e2
  | _ -> false

and compare_block_items expected actual =
  match expected, actual with
  | Statement s1, Statement s2 -> compare_statements s1 s2
  | Decl d1, Decl d2 -> compare_declarations d1 d2
  | _ -> false

let compare_funs expected actual =
  match expected, actual with
  | FunDecl { fun_type=t1; name=name1; params=params1; body=body1 },
    FunDecl { fun_type=t2; name=name2; params=params2; body=body2 } ->
     t1 == t2 && compare_ids name1 name2 && List.for_all2 compare_params params1 params2 && List.for_all2 compare_block_items body1 body2

let compare_asts (Prog fun_list1) (Prog fun_list2) = List.for_all2 compare_funs fun_list1 fun_list2

(* AST construction utilities *)
let make_int i = Const (Int i)

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
let make_ast params block_items =
  let body = block_items in
  let name = ID "main" in
  let f = FunDecl { fun_type=IntType; name; params; body } in
  Prog([f])

let make_simple_ast params exp =
  let ret = ReturnVal exp in
  make_ast params [Statement ret]

(* CONSTANTS *)

let simple_token_list = Lex.lex "int main(){return 2;}"
let simple_ast = make_simple_ast [] (make_int 2)

(* TODO: this probably belongs in a different test group *)
let fun_arg_token_list = Lex.lex "int main(int argc){return 2;}"
let fun_arg_ast =
  let params = [Param (IntType, ID "argc")] in
  let return_exp = make_int 2 in
  make_simple_ast params return_exp

let return_char_tokens = Lex.lex "int main(int argc){return 'a';}"
let return_char_ast =
  let params = [Param (IntType, ID "argc")] in
  let return_exp = Const (Char 'a') in
  make_simple_ast params return_exp

let basic_parse_tests = [
    "test_simple_parse" >:: test_compare_asts simple_token_list simple_ast;
    "test_fun_args" >:: test_compare_asts fun_arg_token_list fun_arg_ast;
    "test_return_char" >:: test_compare_asts return_char_tokens return_char_ast;
  ]

(* UNARY OPERATORS *)

let negation_tokens = Lex.lex "int main(){ return -3;}"
let negation_ast =
  let unop = UnOp (Negate, make_int 3) in
  make_simple_ast [] unop

let pos_tokens = Lex.lex "int main() { return +3;}"
let pos_ast =
  let unop = UnOp (Pos, make_int 3) in
  make_simple_ast [] unop

let complement_tokens = Lex.lex "int main() {return ~3;}"
let complement_ast =
  let unop = UnOp (Complement, make_int 3) in
  make_simple_ast [] unop

let not_tokens = Lex.lex "int main() {return !4;}"
let not_ast =
  let unop = UnOp (Not, make_int 4) in
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
  let binop = BinOp (Add, make_int 1, make_int 2) in
  let params = [] in
  make_simple_ast params binop

let subtraction_tokens = Lex.lex "int main(){return 4-3;}"
let subtraction_ast =
  let binop = BinOp (Sub, make_int 4, make_int 3) in
  make_simple_ast [] binop

let subtract_negative_tokens = Lex.lex "int main(){return 4- -3;}"
let subtract_negative_ast =
  let unop = UnOp (Negate, make_int 3) in
  let binop = BinOp (Sub, make_int 4, unop) in
  make_simple_ast [] binop

let multi_addition_tokens = Lex.lex "int main() {return 1+2+3;}"
let multi_addition_ast =
  let inner_binop = BinOp (Add, make_int 1, make_int 2) in
  let outer_binop = BinOp (Add, inner_binop, make_int 3) in
  let params = [] in
  make_simple_ast params outer_binop

let division_tokens = Lex.lex "int main() {return 4/5;}"
let division_ast =
  let binop = BinOp (Div, make_int 4, make_int 5) in
  make_simple_ast [] binop

let nested_addition_tokens = Lex.lex "int main(){return 1+(2+3);}"
let nested_addition_ast =
  let inner_binop = BinOp (Add, make_int 2, make_int 3) in
  let outer_binop = BinOp (Add, make_int 1, inner_binop) in
  make_simple_ast [] outer_binop

let lots_of_parens_tokens = Lex.lex "int main(){return ((3));}"
let lots_of_parens_ast = make_simple_ast [] (make_int 3)

let left_nested_addition_tokens = Lex.lex "int main() {return (1+2)+3;}"
let left_nested_addition_ast =
  let inner_binop = BinOp (Add, make_int 1, make_int 2) in
  let outer_binop = BinOp (Add, inner_binop, make_int 3) in
  let params = [] in
  make_simple_ast params outer_binop

let mult_tokens = Lex.lex "int main() {return 4*5;}"
let mult_ast =
  let binop = BinOp (Mult, make_int 4, make_int 5) in
  make_simple_ast [] binop

let lots_of_parens_add_tokens = Lex.lex "int main(){return ((1+2));}"
let lots_of_parens_add_ast =
  let e1 = make_int 1 in
  let e2 = make_int 2 in
  let ret_exp = BinOp (Add, e1, e2) in
  make_simple_ast [] ret_exp

let mod_toks = Lex.lex "int main() {return 3%2;}"
let mod_ast =
  let e1 = make_int 3 in
  let e2 = make_int 2 in
  let ret_exp = BinOp (Mod, e1, e2) in
  make_simple_ast [] ret_exp

let precedence_tokens = Lex.lex "int main() {return 1*2+3%2;}"
let precedence_ast =
  let e1 = BinOp (Mult, make_int 1, make_int 2) in
  let e2 = BinOp (Mod, make_int 3, make_int 2) in
  let outer_binop = BinOp (Add, e1, e2) in
  make_simple_ast [] outer_binop

let associativity_tokens = Lex.lex "int main() {return 1/2*3;}"
let associativity_ast =
  let inner_binop = BinOp (Div, make_int 1, make_int 2) in
  let outer_binop = BinOp (Mult, inner_binop, make_int 3) in
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
  let binop = BinOp (BitAnd, make_int 1, make_int 2) in
  make_simple_ast [] binop

let bitwise_or_tokens = Lex.lex "int main() {return 1|2;}"
let bitwise_or_ast =
  let binop = BinOp (BitOr, make_int 1, make_int 2) in
  make_simple_ast [] binop

let xor_tokens = Lex.lex "int main() {return 1^2;}"
let xor_ast =
  let binop = BinOp (Xor, make_int 1, make_int 2) in
  make_simple_ast [] binop

let shiftl_tokens = Lex.lex "int main() {return 1<<2;}"
let shiftl_ast =
  let binop = BinOp (ShiftL, make_int 1, make_int 2) in
  make_simple_ast [] binop

let shiftr_tokens = Lex.lex "int main() {return 1>>2;}"
let shiftr_ast =
  let binop = BinOp (ShiftR, make_int 1, make_int 2) in
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
  let binop = BinOp (And, make_int 1, make_int 2) in
  make_simple_ast [] binop

let or_tokens = Lex.lex "int main() {return 1||2;}"
let or_ast =
  let binop = BinOp (Or, make_int 1, make_int 2) in
  make_simple_ast [] binop

(* addition is higher precedence than &&, which is higher precedence than || *)
let bool_precedence_tokens = Lex.lex "int main() {return 1 || 2 && 3 + 4;}"
let bool_precedence_ast =
  let add_binop = BinOp (Add, make_int 3, make_int 4) in
  let and_binop = BinOp (And, make_int 2, add_binop) in
  let or_binop = BinOp (Or, make_int 1, and_binop) in
  make_simple_ast [] or_binop

let boolean_binop_tests = [
    "test_and" >:: test_compare_asts and_tokens and_ast;
    "test_or" >:: test_compare_asts or_tokens or_ast;
    "test_bool_precedence" >:: test_compare_asts bool_precedence_tokens bool_precedence_ast;
  ]


(* COMPARISON OPERATORS *)

let compare_eq_tokens = Lex.lex "int main() {return 1==2;}"
let compare_eq_ast =
  let binop = BinOp (Eq, make_int 1, make_int 2) in
  make_simple_ast [] binop

let compare_neq_tokens = Lex.lex "int main() {return 1!=2;}"
let compare_neq_ast =
  let binop = BinOp (Neq, make_int 1, make_int 2) in
  make_simple_ast [] binop

let compare_gt_tokens = Lex.lex "int main() {return 1 > 2;}"
let compare_gt_ast =
  let binop = BinOp (Gt, make_int 1, make_int 2) in
  make_simple_ast [] binop

let compare_ge_tokens = Lex.lex "int main() {return 1 >= 2;}"
let compare_ge_ast =
  let binop = BinOp (Ge, make_int 1, make_int 2) in
  make_simple_ast [] binop

let compare_lt_tokens = Lex.lex "int main() {return 1 < 2;}"
let compare_lt_ast =
  let binop = BinOp (Lt, make_int 1, make_int 2) in
  make_simple_ast [] binop

let compare_le_tokens = Lex.lex "int main() {return 1 <= 2;}"
let compare_le_ast =
  let binop = BinOp (Le, make_int 1, make_int 2) in
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
  let decl = Decl { var_type = IntType;
                    var_name = ID "a";
                    init = Some (make_int 2);
                    }
  in
  let ret = Statement (ReturnVal (Var (ID "a"))) in
  let statements = [decl; ret] in
  make_ast [] statements

let assignment_tokens = Lex.lex "int main(){int a; a=2; return 0;}"
let assignment_ast =
  let decl = Decl { var_type = IntType;
                    var_name = ID "a";
                    init = None;
               }
  in
  let assign = Statement (Exp (Assign (Equals, ID "a", make_int 2))) in
  let ret = Statement (ReturnVal (make_int 0)) in
  let block_items = [decl; assign; ret] in
  make_ast [] block_items

let multi_assign_tokens = Lex.lex "int main(){int a; int b = a = 2; return b;}"
let multi_assign_ast =
  let decl_1 = (Decl { var_type = IntType;
                           var_name = ID "a";
                           init = None
               }) in
  let assign = (Assign (Equals, ID "a", make_int 2)) in
  let decl_2 = (Decl { var_type = IntType;
                           var_name = ID "b";
                           init = Some assign
               }) in
  let ret = (Statement (ReturnVal (Var (ID "b")))) in
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
  let cond = make_int 0 in
  let if_body = ReturnVal (make_int 1) in
  let if_statement = (Statement (If { cond; if_body; else_body=None })) in
  let return = (Statement (ReturnVal (make_int 0))) in
  make_ast [] [if_statement; return]

let single_if_else_tokens = Lex.lex "int main(){if (0) return 1; else return 2;}"
let single_if_else_ast =
  let cond = make_int 0 in
  let if_body = ReturnVal (make_int 1) in
  let else_body =  Some (ReturnVal (make_int 2)) in
  let if_statement = (Statement (If { cond; if_body; else_body })) in
  make_ast [] [if_statement]

let ternary_tokens = Lex.lex "int main(){ return 3 < 4 ? 5 : 6; }"
let ternary_ast =
  let condition = BinOp (Lt, make_int 3, make_int 4) in
  let ternop = TernOp (condition, make_int 5, make_int 6) in
  make_simple_ast [] ternop

(* TODO: test nested ternary statements, i'm not totally sure this is right *)

let conditional_parse_tests = [
    "test_single_if" >:: test_compare_asts single_if_tokens single_if_ast;
    "test_single_if_else" >:: test_compare_asts single_if_else_tokens single_if_else_ast;
    "test_ternary" >:: test_compare_asts ternary_tokens ternary_ast
  ]

(* FOR LOOPS *)

let for_tokens = Lex.lex "int main() {for(1; 1; 1) { 1;}}"
let for_ast =
  let const = make_int 1 in
  let for_loop = (Statement (For { init=const; cond=const; post=const; body=Block [(Statement (Exp const))] })) in
  make_ast [] [for_loop]

let for_compound_tokens = Lex.lex "int main() {int a; for(a=0; a<5; a=a+1){ 1+1; if(a<5) {return 3;} } }"
let for_compound_ast =
  let decl = Decl { var_type = IntType; var_name = ID "a"; init = None } in
  let init = Assign(Equals, ID "a", make_int 0) in
  let post = Assign(Equals, ID "a", BinOp (Add, Var(ID "a"), make_int 1)) in
  let cond = BinOp (Lt, Var(ID "a"), make_int 5) in
  let exp = Statement (Exp (BinOp (Add, make_int 1, make_int 1))) in
  let return = Statement (ReturnVal (make_int 3)) in
  let if_statement = Statement (If { cond; if_body=Block [return]; else_body=None }) in
  let body = Block [exp; if_statement] in
  let for_loop = Statement (For { init; cond; post; body }) in
  make_ast [] [decl; for_loop]

let for_declaration_tokens = Lex.lex "int main() {for(int a; 1; 1) { 1;}}"
let for_decl_ast =
  let const = make_int 1 in
  let decl = { var_type = IntType; var_name = ID "a"; init = None} in
  let for_loop = Statement (ForDecl { init=decl; cond=const; post=const; body=Block [Statement (Exp const)] }) in
  make_ast [] [for_loop]

let single_for_tokens = Lex.lex "int main() {for(1; 1; 1) if (1) 1;}"
let single_for_ast =
  let const = make_int 1 in
  let if_statement = If { cond=const; if_body=Exp const; else_body=None } in
  let for_loop = For { init=const; cond=const; post=const; body=if_statement } in
  make_ast [] [Statement for_loop]

let for_parse_tests = [
    "test_for" >:: test_compare_asts for_tokens for_ast;
    "test_for_compound" >:: test_compare_asts for_compound_tokens for_compound_ast;
    "test_for_declaration" >:: test_compare_asts for_declaration_tokens for_decl_ast;
    "test_single_for" >:: test_compare_asts single_for_tokens single_for_ast
  ]

(* FUNCTION CALLS *)
let fun_tokens = Lex.lex "int main() { return foo(); }" (* note: call to undefined function should fail during code generation, not parsing *)
let fun_ast =
  let fun_call = FunCall (ID "foo", []) in
  make_simple_ast [] fun_call

let fun_args_tokens = Lex.lex "int main() { return foo(5); }"
let fun_args_ast =
  let fun_call = FunCall (ID "foo", [make_int 5]) in
  make_simple_ast [] fun_call

let fun_arg_exp_tokens = Lex.lex "int main() { return foo(a + 5); }"
let fun_arg_exp_ast =
  let arg_exp = BinOp (Add, Var (ID "a"), make_int 5) in
  let fun_call = FunCall (ID "foo", [arg_exp]) in
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
    "test_semicolon_required" >:: test_expect_failure missing_semicolon "Expected semicolon after return statement";
    "test_incomplete_addition" >:: test_expect_failure incomplete_addition "Failed to parse factor";
    "test_mismatched_parens" >:: test_expect_failure mismatched_parens "Syntax error: expected close paren";
    "test_mismatched_right_parens" >:: test_expect_failure mismatched_right_parens "Expected semicolon after return statement";
    "test_one_paren" >:: test_expect_failure one_paren "Syntax error: expected close paren";
    "test_backwards_parens" >:: test_expect_failure backwards_parens "Failed to parse factor";
  ]

(* TODO: add comp parse test *)
let parse_tests = basic_parse_tests@unop_parse_tests@binop_parse_tests@boolean_binop_tests
                  @bitwise_binops_tests@comp_parse_tests@variable_parse_tests
                  @conditional_parse_tests@fun_call_parse_tests@for_parse_tests
                  @failure_parse_tests
