open OUnit2

(* create a test to lex a single tokenm *)
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
    "test_lex_id_hash" >:: test_expect_failure "abc#def" "Syntax error: \"#def\" is not valid.";
]

(* NOTE: we do NOT test newline handling because all newlines are removed when we read in the input file *)

let lex_whitespace_tests = [
    "test_leading_whitespace" >:: test_lex_single "   foo" (Lex.Id("foo"));
    "test_leading_tab" >:: test_lex_single "\t foo" (Lex.Id("foo"));
    "test_trailing_whitespace" >:: test_lex_single "-123  " (Lex.Int(-123));
    "test_trailing_tab" >:: test_lex_single "0x81\t" (Lex.Int(0x81));
]

let lex_multi_tests = [
    "test_lex_brace_id" >:: test_lex_multi "}foo" [Lex.CloseBrace; Lex.Id("foo")];
    "test_lex_brace_id_whitespace" >:: test_lex_multi "} bar" [Lex.CloseBrace; Lex.Id("bar")];
    "test_lex_multi_semicolon" >:: test_lex_multi "bar;34" [Lex.Id("bar"); Lex.Semicolon; Lex.Int(34)];
    "test_lex_kw_space" >:: test_lex_multi "return 2" [Lex.ReturnKeyword; Lex.Int(2)];
    "test_lex_kw_nospace" >:: test_lex_multi "return2" [Lex.Id("return2")];
    "test_lex_tab" >:: test_lex_multi "int\tmain" [Lex.IntKeyword; Lex.Id("main")];
    "test_lex_some_spaces" >:: test_lex_multi "under_score main(  a char\t ){ return}; ;\tf;oo"
        [Lex.Id("under_score"); Lex.Id("main"); Lex.OpenParen; Lex.Id("a"); Lex.CharKeyword; Lex.CloseParen;
        Lex.OpenBrace; Lex.ReturnKeyword; Lex.CloseBrace; Lex.Semicolon; Lex.Semicolon; Lex.Id("f");
        Lex.Semicolon; Lex.Id("oo")]
]

let lex_tests = lex_char_tests@lex_int_tests@lex_keyword_tests@lex_punctuation_tests@lex_id_tests@lex_whitespace_tests@lex_multi_tests