open Batteries

let compile prog_filename = 
    let source_lines = File.lines_of prog_filename in
    let source = Enum.reduce (fun line1 line2 -> line1^" "^line2) source_lines in
    let toks = Lex.lex source in
    let ast = Parse.parse toks in
    Gen.generate prog_filename ast

(* TODO: verify .c file extension *)
let filename = Array.get Sys.argv 1

let _ = compile filename