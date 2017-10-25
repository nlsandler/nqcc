open OUnit2

let suite =
    "All" >::: [
        "Lex" >::: Test_lex.lex_tests;
        "Parse" >::: Test_parse.parse_tests
    ]

let () = run_test_tt_main suite