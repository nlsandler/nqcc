open OUnit2

(* create a test to lex a single tokenm *)
let test_lex_single input expected_tok test_ctxt =
    let tokenized = Lex.lex input in
    assert_equal 1 (List.length tokenized);
    assert_equal (expected_tok) (List.hd tokenized)

let test_expect_failure input fail_msg test_ctxt =
    let f =  fun () -> Lex.lex input in
    assert_raises (Failure fail_msg) f

let lex_char_tests = [
    "test_lex_char" >:: test_lex_single "'a'" (Lex.Char('a'));
    "test_lex_char_esc" >:: test_lex_single "'\\n'" (Lex.Char('\n')); (* escape sequence - newline *)
    "test_lex_char_backslash" >:: test_lex_single "'\\\\'" (Lex.Char('\\')); (* escape sequence - backslash *)
    "test_lex_char_hex" >:: test_lex_single "'\\x61'" (Lex.Char('a')); (* hex *)
    "test_lex_char_octal" >:: test_lex_single "'\\141'" (Lex.Char('a')); (* octal *)
    "test_lex_char_too_long" >:: test_expect_failure "'aa'" "Syntax error: \"'aa'\" is not valid.";
]

let lex_int_tests = [
    "test_lex_int" >:: test_lex_single "3" (Lex.Int(3));
    "test_lex_int_neg" >:: test_lex_single "-3" (Lex.Int(-3));
    "test_lex_int_max" >:: test_lex_single "2147483647" (Lex.Int(2147483647));
    "test_lex_int_overflow" >:: test_expect_failure "2147483648" "Invalid int literal";
    "test_lex_int_min" >:: test_lex_single "-2147483648" (Lex.Int(-2147483648));
    "test_lex_int_underflow" >:: test_expect_failure "-2147483649" "Invalid int literal";
    "test_lex_int_single_digit_hex" >:: test_lex_single "0xf" (Lex.Int(0xf));
    "test_lex_int_multi_digit_hex" >:: test_lex_single "0xaf42" (Lex.Int(0xaf42));
    "test_lex_int_octal" >:: test_lex_single "0666" (Lex.Int(0o666));
    "test_lex_int_octal_overflow" >:: test_expect_failure "020000000000" "Invalid int literal";
    "test_lex_int_hex_overflow" >:: test_expect_failure "0x80000000" "Invalid int literal";
]

let lex_keyword_tests = [
    "test_lex_return_keyword" >:: test_lex_single "return" (Lex.ReturnKeyword);
    "test_lex_char_keyword" >:: test_lex_single "char" (Lex.CharKeyword);
    "test_lex_int_keyword" >:: test_lex_single "int" (Lex.IntKeyword);
]

let lex_punctuation_tests = [
    "test_lex_openbrace" >:: test_lex_single "{" (Lex.OpenBrace);
    "test_lex_closebrace" >:: test_lex_single "}" (Lex.CloseBrace);
    "test_lex_openparen" >:: test_lex_single "(" (Lex.OpenParen);
    "test_lex_closeparen" >:: test_lex_single ")" (Lex.CloseParen);
    "test_lex_semicolon" >:: test_lex_single ";" (Lex.Semicolon);
]

let lex_id_tests = [
    "test_lex_id_simple" >:: test_lex_single "hello" (Lex.Id("hello"));
    "test_lex_id_underscore" >:: test_lex_single "_hell_o" (Lex.Id("_hell_o"));
    "test_lex_id_uppercase" >:: test_lex_single "HELLO" (Lex.Id("HELLO"));
    "test_lex_id_uppercase_return" >:: test_lex_single "RETURN" (Lex.Id("RETURN"));
    "test_lex_id_uppercase_char" >:: test_lex_single "CHAR" (Lex.Id("CHAR"));
    "test_lex_id_uppercase_int" >:: test_lex_single "INT" (Lex.Id("INT"));
    "test_lex_id_numeric" >:: test_lex_single "a123" (Lex.Id("a123"));
    "test_lex_id_numeric_start" >:: test_expect_failure "123a" "Syntax error: \"123a\" is not valid.";
    "test_lex_id_hyphen" >:: test_expect_failure "abc-def" "Syntax error: \"-def\" is not valid."; (* TODO: Valid in the future *)
    "test_lex_id_at" >:: test_expect_failure "abc@def" "Syntax error: \"@def\" is not valid.";
    "test_lex_id_money" >:: test_expect_failure "abc$def" "Syntax error: \"$def\" is not valid.";
]

(*
    | Id of string
*)

let lex_tests = lex_char_tests@lex_int_tests@lex_keyword_tests@lex_punctuation_tests@lex_id_tests