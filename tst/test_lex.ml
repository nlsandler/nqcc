open OUnit2

(* create a test to lex a single tokenm *)
let test_lex_single input expected_tok test_ctxt =
    let tokenized = Lex.lex input in
    assert_equal 1 (List.length tokenized);
    assert_equal (expected_tok) (List.hd tokenized)

let lex_char_tests = [
    "test_lex_char" >:: test_lex_single "'a'" (Lex.Char('a'));
    "test_lex_char_esc" >:: test_lex_single "'\\n'" (Lex.Char('\n')); (* escape sequence - newline *)
    "test_lex_char_backslash" >:: test_lex_single "'\\\\'" (Lex.Char('\\')); (* escape sequence - backslash *)
    "test_lex_char_hex" >:: test_lex_single "'\\x61'" (Lex.Char('a')); (* hex *)
    "test_lex_char_octal" >:: test_lex_single "'\\141'" (Lex.Char('a')) (* octal *)
]

let lex_tests = lex_char_tests