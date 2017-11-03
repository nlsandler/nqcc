let type_to_string fun_type =
    match fun_type with
    | Ast.IntType -> "INT"
    | Ast.CharType -> "CHAR"

let id_to_string (Ast.ID(id)) = id

let param_to_string (Ast.Param(param_type, param_id)) = Printf.sprintf "%s %s" (type_to_string param_type) (id_to_string param_id)

let pprint_function_decl fun_type fun_id =
    Printf.printf "FUN %s %s:\n" (type_to_string fun_type) (id_to_string fun_id)

let pprint_function_params params =
    let param_strings = List.map param_to_string params in
    let param_list_string = String.concat ", " param_strings in
    Printf.printf "\tparams: (%s)\n" param_list_string

let const_to_string = function
    | Ast.Int i ->  Printf.sprintf "Int<%d>" i
    | Ast.Char c -> Printf.sprintf "Char<%c>" c
    | Ast.String s -> Printf.sprintf "String<%s>" s

let op_to_string op =
    match op with
    | Ast.Add -> "+"
    | Ast.Mult -> "*"
    | Ast.Div -> "/"

let rec exp_to_string = function
    | Ast.Const c -> const_to_string c
    | Ast.BinOp(op, e1, e2) -> Printf.sprintf "(%s %s %s)" (exp_to_string e1) (op_to_string op) (exp_to_string e2)

let pprint_stmt = function
    | Ast.Return -> print_string "\t\tRETURN\n"
    | Ast.ReturnVal(e) -> Printf.printf "\t\t RETURN %s\n" (exp_to_string e)

let pprint_function_body (Ast.Body(stmts)) =
    print_string "\tbody:\n";
    List.map pprint_stmt stmts

let pprint (Ast.Prog p) = 
    match p with
    | Ast.FunDecl(fun_type, fun_id, fun_params, fun_body) ->
        let _ = pprint_function_decl fun_type fun_id;
            pprint_function_params fun_params;
            pprint_function_body fun_body; in
        ()