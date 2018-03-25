open OUnit2
open Tok

(* create a test to lex a single token *)
let test_lex_single input expected_tok test_ctxt =
  let tokenized = Lex.lex input in
  assert_equal 1 (List.length tokenized);
  assert_equal (expected_tok) (List.hd tokenized)

let test_lex_multi input expected_toks test_ctxt =
  let tokenized = Lex.lex input in
  let rec compare_token_lists a b =
    match a with
    | [] -> assert_equal b []
    | head::tail -> assert_equal (List.hd b) head; compare_token_lists tail (List.tl b) in
  compare_token_lists tokenized expected_toks

let test_expect_failure input fail_msg test_ctxt =
  let f =  fun () -> Lex.lex input in
  assert_raises (Failure fail_msg) f

let lex_char_tests =
  [
    "test_lex_char" >:: test_lex_single "'a'" (Char 'a');
    "test_lex_char_esc" >:: test_lex_single "'\\n'" (Char '\n'); (* escape sequence - newline *)
    "test_lex_char_backslash" >:: test_lex_single "'\\\\'" (Char '\\'); (* escape sequence - backslash *)
    "test_lex_char_hex" >:: test_lex_single "'\\x61'" (Char 'a'); (* hex *)
    "test_lex_char_octal" >:: test_lex_single "'\\141'" (Char 'a'); (* octal *)
    "test_lex_char_too_long" >:: test_expect_failure "'aa'" "Syntax error: \"'aa'\" is not valid.";
  ]

let lex_int_tests =
  [
    "test_lex_int" >:: test_lex_single "3" (Int 3);
    "test_lex_zero" >:: test_lex_single "0" (Int 0);
    "test_lex_int_max" >:: test_lex_single "2147483647" (Int 2147483647);
    "test_lex_int_overflow" >:: test_expect_failure "2147483648" "Invalid int literal";
    "test_lex_int_underflow" >:: test_expect_failure "-2147483649" "Invalid int literal";
    "test_lex_int_single_digit_hex" >:: test_lex_single "0xf" (Int 0xf);
    "test_lex_int_multi_digit_hex" >:: test_lex_single "0xaf42" (Int 0xaf42);
    "test_lex_int_octal" >:: test_lex_single "0666" (Int 0o666);
    "test_lex_int_octal_overflow" >:: test_expect_failure "020000000000" "Invalid int literal";
    "test_lex_int_hex_overflow" >:: test_expect_failure "0x80000000" "Invalid int literal";
  ]

let lex_keyword_tests =
  [
    "test_lex_return_keyword" >:: test_lex_single "return" ReturnKeyword;
    "test_lex_char_keyword" >:: test_lex_single "char" CharKeyword;
    "test_lex_int_keyword" >:: test_lex_single "int" IntKeyword;
    "test_lex_if_keyword" >:: test_lex_single "if" IfKeyword;
    "test_lex_else_keyword" >:: test_lex_single "else" ElseKeyword;
    "test_lex_for_keyword" >:: test_lex_single "for" ForKeyword;
    "test_lex_while_keyword" >:: test_lex_single "while" WhileKeyword;
    "test_lex_do_keyword" >:: test_lex_single "do" DoKeyword;
  ]

let lex_punctuation_tests = [
    "test_lex_openbrace" >:: test_lex_single "{" OpenBrace;
    "test_lex_closebrace" >:: test_lex_single "}" CloseBrace;
    "test_lex_openparen" >:: test_lex_single "(" OpenParen;
    "test_lex_closeparen" >:: test_lex_single ")" CloseParen;
    "test_lex_semicolon" >:: test_lex_single ";" Semicolon;
    "test_lex_comma" >:: test_lex_single "," Comma;
    "test_lex_plus" >:: test_lex_single "+" Plus;
    "test_lex_minus" >:: test_lex_single "-" Minus;
    "test_lex_product" >:: test_lex_single "*" Mult;
    "test_lex_divide" >:: test_lex_single "/" Div;
    "test_lex_modulo" >:: test_lex_single "%" Mod;
    "test_lex_complement" >:: test_lex_single "~" Complement;
    "test_lex_bang" >:: test_lex_single "!" Bang;
    "test_lex_eq" >:: test_lex_single "=" Eq;
    "test_lex_double_eq" >:: test_lex_single "==" DoubleEq;
    "test_lex_neq" >:: test_lex_single "!=" Neq;
    "test_lex_gt" >:: test_lex_single ">" Gt;
    "test_lex_ge" >:: test_lex_single ">=" Ge;
    "test_lex_lt" >:: test_lex_single "<" Lt;
    "test_lex_le" >:: test_lex_single "<=" Le;
    "test_lex_and" >:: test_lex_single "&&" And;
    "test_lex_or" >:: test_lex_single "||" Or;
    "test_lex_bitwise_and" >:: test_lex_single "&" BitAnd;
    "test_lex_bitwise_or" >:: test_lex_single "|" BitOr;
    "test_lex_xor" >:: test_lex_single "^" Xor;
    "test_lex_shiftl" >:: test_lex_single "<<" ShiftLeft;
    "test_lex_shiftr" >:: test_lex_single ">>" ShiftRight;
    "test_lex_question" >:: test_lex_single "?" Question;
    "test_lex_colon" >:: test_lex_single ":" Colon
  ]

let lex_id_tests = [
    "test_lex_id_simple" >:: test_lex_single "hello" (Id "hello");
    "test_lex_id_underscore" >:: test_lex_single "_hell_o" (Id "_hell_o");
    "test_lex_id_uppercase" >:: test_lex_single "HELLO" (Id "HELLO");
    "test_lex_id_uppercase_return" >:: test_lex_single "RETURN" (Id "RETURN");
    "test_lex_id_uppercase_char" >:: test_lex_single "CHAR" (Id "CHAR");
    "test_lex_id_uppercase_int" >:: test_lex_single "INT" (Id "INT");
    "test_lex_id_numeric" >:: test_lex_single "a123" (Id "a123");
    "test_lex_id_numeric_start" >:: test_expect_failure "123a" "Syntax error: \"123a\" is not valid.";
    "test_lex_id_at" >:: test_expect_failure "abc@def" "Syntax error: \"@def\" is not valid.";
    "test_lex_id_money" >:: test_expect_failure "abc$def" "Syntax error: \"$def\" is not valid.";
    "test_lex_id_hash" >:: test_expect_failure "abc#def" "Syntax error: \"#def\" is not valid.";
  ]

(* NOTE: we do NOT test newline handling because all newlines are removed when we read in the input file *)

let lex_whitespace_tests = [
    "test_leading_whitespace" >:: test_lex_single "   foo" (Id "foo");
    "test_leading_tab" >:: test_lex_single "\t foo" (Id "foo");
    "test_trailing_whitespace" >:: test_lex_single "123  " (Int 123);
    "test_trailing_tab" >:: test_lex_single "0x81\t" (Int 0x81);
  ]

let lex_multi_tests = [
    "test_lex_negative" >:: test_lex_multi "-1" [Minus; Int 1];
    "test_lex_positive" >:: test_lex_multi "+1" [Plus; Int 1];
    "test_lex_brace_id" >:: test_lex_multi "}foo" [CloseBrace; Id "foo"];
    "test_lex_brace_id_whitespace" >:: test_lex_multi "} bar" [CloseBrace; Id "bar"];
    "test_lex_multi_semicolon" >:: test_lex_multi "bar;34" [Id "bar"; Semicolon; Int 34];
    "test_lex_kw_space" >:: test_lex_multi "return 2" [ReturnKeyword; Int 2];
    "test_lex_kw_nospace" >:: test_lex_multi "return2" [Id "return2"];
    "test_lex_tab" >:: test_lex_multi "int\tmain" [IntKeyword; Id "main"];
    "test_lex_some_spaces" >:: test_lex_multi "under_score main(  a char\t ){ return}; ;\tf;oo"
                                 [Id "under_score"; Id "main"; OpenParen; Id "a"; CharKeyword; CloseParen;
                                  OpenBrace; ReturnKeyword; CloseBrace; Semicolon; Semicolon; Id "f";
                                  Semicolon; Id "oo"];
    "test_lex_addition" >:: test_lex_multi "a+b" [Id "a"; Plus; Id "b"];
    "test_lex_addition_spaces" >:: test_lex_multi "1 + b" [Int 1; Plus; Id "b"];
    "test_lex_addition_parens" >:: test_lex_multi "3 +('c'+ b)" [Int 3; Plus; OpenParen; Char 'c'; Plus; Id "b"; CloseParen];
    "test_lex_subtraction" >:: test_lex_multi "4 - 'a'" [Int 4; Minus; Char 'a'];
    "test_lex_multiplication" >:: test_lex_multi "2*2" [Int 2; Mult; Int 2];
    "test_lex_division" >:: test_lex_multi "2/2" [Int 2; Div; Int 2];
    "test_lex_complement" >:: test_lex_multi "~5" [Complement; Int 5];
    "test_lex_compl_var" >:: test_lex_multi "~var" [Complement; Id "var"];
    "test_lex_bang" >:: test_lex_multi "!foo" [Bang; Id "foo"];
    "test_lex_assignment" >:: test_lex_multi "a=5" [Id "a"; Eq; Int 5];
    "test_lex_declaration" >:: test_lex_multi "int somevar;" [IntKeyword; Id "somevar"; Semicolon];
    "test_lex_equals" >:: test_lex_multi "a == 4" [Id "a"; DoubleEq; Int 4];
    "test_lex_not_equal" >:: test_lex_multi "a != 4" [Id "a"; Neq; Int 4];
  ]

let lex_tests = lex_char_tests@lex_int_tests@lex_keyword_tests@lex_punctuation_tests@lex_id_tests@lex_whitespace_tests@lex_multi_tests
