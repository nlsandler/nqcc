open Ast

let type_to_string fun_type =
  match fun_type with
  | IntType -> "INT"
  | CharType -> "CHAR"

let id_to_string (ID id) = id

let param_to_string (Param (param_type, param_id)) = Printf.sprintf "%s %s" (type_to_string param_type) (id_to_string param_id)

let pprint_function_decl fun_type fun_id =
  Printf.printf "FUN %s %s:\n" (type_to_string fun_type) (id_to_string fun_id)

let pprint_function_params params =
  let param_strings = List.map param_to_string params in
  let param_list_string = String.concat ", " param_strings in
  Printf.printf "\tparams: (%s)\n" param_list_string

let const_to_string = function
  | Int i ->  Printf.sprintf "Int<%d>" i
  | Char c -> Printf.sprintf "Char<%c>" c
  | String s -> Printf.sprintf "String<%s>" s

let op_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Neq -> "!="
  | Eq -> "=="
  | And -> "&&"
  | Or -> "||"
  | BitAnd -> "&"
  | BitOr -> "|"
  | Xor -> "^"
  | ShiftL -> "<<"
  | ShiftR -> ">>"

let unop_to_string = function
  | Negate -> "-"
  | Pos -> "+"
  | Complement -> "~"
  | Not -> "!"

let assign_op_to_string = function
  | Equals -> "="

let rec exp_to_string = function
  | Var (ID v) -> Printf.sprintf "VAR<%s>" v
  | Const c -> const_to_string c
  | TernOp (e1, e2, e3) -> Printf.sprintf "%s ? %s : %s" (exp_to_string e1) (exp_to_string e2) (exp_to_string e3)
  | BinOp (op, e1, e2) -> Printf.sprintf "(%s %s %s)" (exp_to_string e1) (op_to_string op) (exp_to_string e2)
  | UnOp (op, e) -> Printf.sprintf "(%s %s)" (unop_to_string op) (exp_to_string e)
  | FunCall (fun_id, args) -> Printf.sprintf "%s(%s)" (id_to_string fun_id) (args_to_string args)
  | Assign (op, ID var_name, rhs) ->
     Printf.sprintf "%s %s %s" var_name (assign_op_to_string op) (exp_to_string rhs)

and args_to_string args =
  let arg_strings = List.map exp_to_string args in
  String.concat ", " arg_strings

let optional_exp_to_string = function
  | Some e -> exp_to_string e
  | None -> ""

let decl_to_string { var_type; var_name=ID id; init } =
  let decl_str = Printf.sprintf "%s %s" (type_to_string var_type) id in
  match init with
  | None -> decl_str
  | Some e -> Printf.sprintf "%s = %s" decl_str (exp_to_string e)

let pprint_decl indent decl = Printf.printf "%s%s\n" indent (decl_to_string decl)

let rec pprint_block_item indent = function
  | Decl d -> pprint_decl indent d
  | Statement s -> pprint_stmt indent s

and pprint_stmt indent = function
  | ReturnVal e -> Printf.printf "%sRETURN %s\n" indent (exp_to_string e)
  | Block block_items -> List.iter (fun item -> pprint_block_item indent item) block_items
  | If { cond; if_body; else_body } ->
     Printf.printf "%sIF (%s)\n" indent (exp_to_string cond);
     pprint_stmt (indent^"\t") if_body;
     begin match else_body with
     | Some statement ->
        Printf.printf "%sELSE\n" indent;
        pprint_stmt (indent^"\t") statement;
        Printf.printf "\n"
     | None -> Printf.printf "\n"
     end
  | For { init; cond; post; body } ->
     Printf.printf "%sFOR (%s ; %s ; %s)\n" indent
       (optional_exp_to_string init)
       (exp_to_string cond)
       (optional_exp_to_string post);
     pprint_stmt (indent^"\t") body;
     Printf.printf "\n"
  | ForDecl { init; cond; post; body } ->
     Printf.printf "%sFOR (%s ; %s ; %s)\n" indent
       (decl_to_string init) (exp_to_string cond) (optional_exp_to_string post);
     pprint_stmt (indent^"\t") body;
     Printf.printf "\n"
  | Exp e -> Printf.printf "%s%s\n" indent (optional_exp_to_string e)
  | While { cond; body } ->
     Printf.printf "%sWHILE (%s):\n" (exp_to_string cond) indent;
     pprint_stmt (indent^"\t") body;
     Printf.printf "\n"
  | DoWhile { body; cond } ->
     Printf.printf "%sDO:\n" indent;
     pprint_stmt (indent^"\t") body;
     Printf.printf "%sWHILE (%s)\n" (exp_to_string cond) indent

let pprint_function_body body =
  print_string "\tbody:\n";
  List.map (fun item -> pprint_block_item "\t\t" item) body

let pprint_function (FunDecl { fun_type; name; params; body }) =
  let _ = pprint_function_decl fun_type name in
  let _ = pprint_function_params params in
  let _ = pprint_function_body body in
  ()

let pprint (Prog funs) = List.iter pprint_function funs
