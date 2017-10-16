open Lex 
open Parse

let toks = Lex.lex "int main() { return 2; }" 
let tok_strs = List.map Lex.tok_to_string toks
let _ = List.iter (Printf.printf "%s ," ) tok_strs
let ast = Parse.parse toks
