module AST =
    struct
        type const = 
            | Int of int
            | Char of char
            | String of string
        type type_def = 
            | IntType
            | CharType
        type exp = Const of const
        type statement = 
            | Return
            | ReturnVal of exp (* should we add a return_exp instead? *)
        type id = ID of string
        type fun_param = Param of type_def * id
        type fun_body = Body of statement list
        type fun_decl = FunDecl of type_def * id * fun_param list * fun_body
        type prog = Prog of fun_decl
(*
        let rec ast_to_str depth node =
            let tabs = String.make depth '\t' in
            match node with
            | Prog p -> print_endline (tabs^"Prog:"); ast_to_str (depth + 1) p
            | FunDecl fun_type, fun_name, params, body -> print_endline (tabs^"Function:");
                ast_to_str (depth + 1) fun_type;
                ast_to_str (depth + 1) fun_name;
                ast_to_str (depth + 1) params;
                ast_to_str (depth + 1) body;
            | Body statements -> print_endline (tabs^"Body:");
                List.iter (ast_to_str (depth + 1)) statements
            | 
*)
    end