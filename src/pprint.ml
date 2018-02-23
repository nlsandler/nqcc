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

let op_to_string = function
    | Ast.Add -> "+"
    | Ast.Sub -> "-"
    | Ast.Mult -> "*"
    | Ast.Div -> "/"
    | Ast.Mod -> "%"
    | Ast.Lt -> "<"
    | Ast.Le -> "<="
    | Ast.Gt -> ">"
    | Ast.Ge -> ">="
    | Ast.Neq -> "!="
    | Ast.Eq -> "=="
    | Ast.And -> "&&"
    | Ast.Or -> "||"
    | Ast.BitAnd -> "&"
    | Ast.BitOr -> "|"
    | Ast.Xor -> "^"
    | Ast.ShiftL -> "<<"
    | Ast.ShiftR -> ">>"

let unop_to_string = function
    | Ast.Negate -> "-"
    | Ast.Pos -> "+"
    | Ast.Complement -> "~"
    | Ast.Not -> "!"

let assign_op_to_string = function
    | Ast.Equals -> "="

let rec exp_to_string = function
    | Ast.Var(Ast.ID v) -> Printf.sprintf "VAR<%s>" v
    | Ast.Const c -> const_to_string c
    | Ast.TernOp (e1, e2, e3) -> Printf.sprintf "%s ? %s : %s" (exp_to_string e1) (exp_to_string e2) (exp_to_string e3)
    | Ast.BinOp(op, e1, e2) -> Printf.sprintf "(%s %s %s)" (exp_to_string e1) (op_to_string op) (exp_to_string e2)
    | Ast.UnOp(op, e) -> Printf.sprintf "(%s %s)" (unop_to_string op) (exp_to_string e)
    | Ast.FunCall(fun_id, args) -> Printf.sprintf "%s(%s)" (id_to_string fun_id) (args_to_string args)
    | Ast.Assign(op, Ast.ID(var_name), rhs) ->
        Printf.sprintf "%s %s %s\n" var_name (assign_op_to_string op) (exp_to_string rhs)

and args_to_string args =
    let arg_strings = List.map exp_to_string args in
    String.concat ", " arg_strings

let pprint_decl Ast.{ var_type; var_name=ID(id); init } =
    match init with
    | None -> Printf.printf "\t\t%s %s\n" (type_to_string var_type) id
    | Some e -> Printf.printf "\t\t%s %s = %s\n" (type_to_string var_type) id (exp_to_string e)

let rec pprint_stmt = function
    | Ast.Decl(decl) -> pprint_decl decl
    | Ast.ReturnVal(e) -> Printf.printf "\t\tRETURN %s\n" (exp_to_string e)
    | Ast.Block statements -> begin
        Printf.printf "{";
        List.iter pprint_stmt statements;
        Printf.printf "}"
      end
    | Ast.If(cond, then_body, else_body) ->
        Printf.printf "\t\tIF (%s) {\n" (exp_to_string cond);
        pprint_stmt then_body;
        begin match else_body with
        | Some statement ->
            Printf.printf "\t\t} ELSE {\n";
            pprint_stmt statement;
            Printf.printf "\t\t}\n"
        | None -> Printf.printf "\t\t}\n"
        end
    | Ast.(For { init; cond; post; body }) ->
        Printf.printf "\t\tFOR (%s ; %s ; %s) {\n"
            (exp_to_string init) (exp_to_string cond) (exp_to_string post);
        pprint_stmt body;
        Printf.printf"\t\t}\n"
    | Ast.(ForDecl { init; cond; post; body }) ->
        Printf.printf "\t\tFOR ( ";
        pprint_decl init;
        Printf.printf " ; %s ; %s ) {\n" (exp_to_string cond) (exp_to_string post);
        pprint_stmt body;
        Printf.printf"\t\t}\n";
    | Ast.Exp(e) -> Printf.printf "%s" (exp_to_string e)

let pprint_function_body (Ast.Body(stmts)) =
    print_string "\tbody:\n";
    List.map pprint_stmt stmts

let pprint_function (Ast.FunDecl(fun_type, fun_id, fun_params, fun_body)) =
    let _ = pprint_function_decl fun_type fun_id in
    let _ = pprint_function_params fun_params in
    let _ = pprint_function_body fun_body in
    ()

let pprint (Ast.Prog funs) = List.iter pprint_function funs
