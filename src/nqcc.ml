open Lex 
open Parse
open Gen
open Batteries

let compile prog_filename = 
    let source_lines = File.lines_of prog_filename in
    let source = Enum.reduce (fun line1 line2 -> line1^" "^line2) source_lines in
    let _ = Printf.printf "%s" source in
    let toks = Lex.lex source in
    let tok_strs = List.map Lex.tok_to_string toks in
    let _ = List.iter (Printf.printf "%s ," ) tok_strs in
    let ast = Parse.parse toks in
    Gen.generate prog_filename ast

(* TODO: verify .c file extension *)
let filename = Array.get Sys.argv 1

let _ = compile filename