(* Data types representing an abstract syntax tree *)
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