open OUnit2

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

let lex_char_tests = [
    "test_lex_char" >:: test_lex_single "'a'" (Tok.Char('a'));
    "test_lex_char_esc" >:: test_lex_single "'\\n'" (Tok.Char('\n')); (* escape sequence - newline *)
    "test_lex_char_backslash" >:: test_lex_single "'\\\\'" (Tok.Char('\\')); (* escape sequence - backslash *)
    "test_lex_char_hex" >:: test_lex_single "'\\x61'" (Tok.Char('a')); (* hex *)
    "test_lex_char_octal" >:: test_lex_single "'\\141'" (Tok.Char('a')); (* octal *)
    "test_lex_char_too_long" >:: test_expect_failure "'aa'" "Syntax error: \"'aa'\" is not valid.";
]

let lex_int_tests = [
    "test_lex_int" >:: test_lex_single "3" (Tok.Int(3));
    "test_lex_zero" >:: test_lex_single "0" (Tok.Int(0));
    "test_lex_int_max" >:: test_lex_single "2147483647" (Tok.Int(2147483647));
    "test_lex_int_overflow" >:: test_expect_failure "2147483648" "Invalid int literal";
    "test_lex_int_underflow" >:: test_expect_failure "-2147483649" "Invalid int literal";
    "test_lex_int_single_digit_hex" >:: test_lex_single "0xf" (Tok.Int(0xf));
    "test_lex_int_multi_digit_hex" >:: test_lex_single "0xaf42" (Tok.Int(0xaf42));
    "test_lex_int_octal" >:: test_lex_single "0666" (Tok.Int(0o666));
    "test_lex_int_octal_overflow" >:: test_expect_failure "020000000000" "Invalid int literal";
    "test_lex_int_hex_overflow" >:: test_expect_failure "0x80000000" "Invalid int literal";
]

let lex_keyword_tests = [
    "test_lex_return_keyword" >:: test_lex_single "return" (Tok.ReturnKeyword);
    "test_lex_char_keyword" >:: test_lex_single "char" (Tok.CharKeyword);
    "test_lex_int_keyword" >:: test_lex_single "int" (Tok.IntKeyword);
]

let lex_punctuation_tests = [
    "test_lex_openbrace" >:: test_lex_single "{" (Tok.OpenBrace);
    "test_lex_closebrace" >:: test_lex_single "}" (Tok.CloseBrace);
    "test_lex_openparen" >:: test_lex_single "(" (Tok.OpenParen);
    "test_lex_closeparen" >:: test_lex_single ")" (Tok.CloseParen);
    "test_lex_semicolon" >:: test_lex_single ";" (Tok.Semicolon);
    "test_lex_plus" >:: test_lex_single "+" (Tok.Plus);
    "test_lex_minus" >:: test_lex_single "-" (Tok.Minus);
    "test_lex_product" >:: test_lex_single "*" (Tok.Mult);
    "test_lex_divide" >:: test_lex_single "/" (Tok.Div);
    "test_lex_complement" >:: test_lex_single "~" (Tok.Complement);
    "test_lex_bang" >:: test_lex_single "!" (Tok.Bang);
    "test_lex_eq" >:: test_lex_single "=" (Tok.Eq)
]

let lex_id_tests = [
    "test_lex_id_simple" >:: test_lex_single "hello" (Tok.Id("hello"));
    "test_lex_id_underscore" >:: test_lex_single "_hell_o" (Tok.Id("_hell_o"));
    "test_lex_id_uppercase" >:: test_lex_single "HELLO" (Tok.Id("HELLO"));
    "test_lex_id_uppercase_return" >:: test_lex_single "RETURN" (Tok.Id("RETURN"));
    "test_lex_id_uppercase_char" >:: test_lex_single "CHAR" (Tok.Id("CHAR"));
    "test_lex_id_uppercase_int" >:: test_lex_single "INT" (Tok.Id("INT"));
    "test_lex_id_numeric" >:: test_lex_single "a123" (Tok.Id("a123"));
    "test_lex_id_numeric_start" >:: test_expect_failure "123a" "Syntax error: \"123a\" is not valid.";
    "test_lex_id_at" >:: test_expect_failure "abc@def" "Syntax error: \"@def\" is not valid.";
    "test_lex_id_money" >:: test_expect_failure "abc$def" "Syntax error: \"$def\" is not valid.";
    "test_lex_id_hash" >:: test_expect_failure "abc#def" "Syntax error: \"#def\" is not valid.";
]

(* NOTE: we do NOT test newline handling because all newlines are removed when we read in the input file *)

let lex_whitespace_tests = [
    "test_leading_whitespace" >:: test_lex_single "   foo" (Tok.Id("foo"));
    "test_leading_tab" >:: test_lex_single "\t foo" (Tok.Id("foo"));
    "test_trailing_whitespace" >:: test_lex_single "123  " (Tok.Int(123));
    "test_trailing_tab" >:: test_lex_single "0x81\t" (Tok.Int(0x81));
]

let lex_multi_tests = [
    "test_lex_negative" >:: test_lex_multi "-1" [Tok.Minus; Tok.Int(1)];
    "test_lex_positive" >:: test_lex_multi "+1" [Tok.Plus; Tok.Int(1)];
    "test_lex_brace_id" >:: test_lex_multi "}foo" [Tok.CloseBrace; Tok.Id("foo")];
    "test_lex_brace_id_whitespace" >:: test_lex_multi "} bar" [Tok.CloseBrace; Tok.Id("bar")];
    "test_lex_multi_semicolon" >:: test_lex_multi "bar;34" [Tok.Id("bar"); Tok.Semicolon; Tok.Int(34)];
    "test_lex_kw_space" >:: test_lex_multi "return 2" [Tok.ReturnKeyword; Tok.Int(2)];
    "test_lex_kw_nospace" >:: test_lex_multi "return2" [Tok.Id("return2")];
    "test_lex_tab" >:: test_lex_multi "int\tmain" [Tok.IntKeyword; Tok.Id("main")];
    "test_lex_some_spaces" >:: test_lex_multi "under_score main(  a char\t ){ return}; ;\tf;oo"
        [Tok.Id("under_score"); Tok.Id("main"); Tok.OpenParen; Tok.Id("a"); Tok.CharKeyword; Tok.CloseParen;
        Tok.OpenBrace; Tok.ReturnKeyword; Tok.CloseBrace; Tok.Semicolon; Tok.Semicolon; Tok.Id("f");
        Tok.Semicolon; Tok.Id("oo")];
    "test_lex_addition" >:: test_lex_multi "a+b" [Tok.Id("a"); Tok.Plus; Tok.Id("b")];
    "test_lex_addition_spaces" >:: test_lex_multi "1 + b" [Tok.Int(1); Tok.Plus; Tok.Id("b")];
    "test_lex_addition_parens" >:: test_lex_multi "3 +('c'+ b)" [Tok.Int(3); Tok.Plus; Tok.OpenParen; Tok.Char('c'); Tok.Plus; Tok.Id("b"); Tok.CloseParen];
    "test_lex_subtraction" >:: test_lex_multi "4 - 'a'" [Tok.Int(4); Tok.Minus; Tok.Char('a')];
    "test_lex_multiplication" >:: test_lex_multi "2*2" [Tok.Int(2); Tok.Mult; Tok.Int(2)];
    "test_lex_division" >:: test_lex_multi "2/2" [Tok.Int(2); Tok.Div; Tok.Int(2)];
    "test_lex_complement" >:: test_lex_multi "~5" [Tok.Complement; Tok.Int(5)];
    "test_lex_compl_var" >:: test_lex_multi "~var" [Tok.Complement; Tok.Id("var")];
    "test_lex_bang" >:: test_lex_multi "!foo" [Tok.Bang; Tok.Id("foo")];
    "test_lex_assignment" >:: test_lex_multi "a=5" [Tok.Id("a"); Tok.Eq; Tok.Int(5);];
    "test_lex_declaration" >:: test_lex_multi "int somevar;" [Tok.IntKeyword; Tok.Id("somevar"); Tok.Semicolon];
]

let lex_tests = lex_char_tests@lex_int_tests@lex_keyword_tests@lex_punctuation_tests@lex_id_tests@lex_whitespace_tests@lex_multi_tests