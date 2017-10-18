open Lex 
open Parse
open Gen

let toks = Lex.lex "int main() { return 'a'; }" 
let tok_strs = List.map Lex.tok_to_string toks
let _ = List.iter (Printf.printf "%s ," ) tok_strs
let ast = Parse.parse toks
let _ = Gen.generate ast